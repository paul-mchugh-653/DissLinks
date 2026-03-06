open CommonTypes
open Utility

let connection_info
  = let setting =
      Settings.(option "database_args"
              |> synopsis "Database host, port, user, and password"
              |> to_string from_string_option
              |> convert (fun s -> Utility.some (Sys.expand s))
              |> sync)
    in setting

(* Hacky database query result manipulation settings. *)
let coerce_null_integers
  = Settings.(flag "coerce_null_integers"
              |> convert parse_bool
              |> sync)

let null_integer
  = Settings.(option ~default:(Some (-1)) "null_integer"
              |> to_string (function Some i -> string_of_int i | None -> "<none>")
              |> convert (fun s -> Some (int_of_string s))
              |> sync)

let mixing_norm
  = Settings.(flag "mixing_norm"
              |> synopsis "Enables the new mixing normaliser for all queries"
              |> convert parse_bool
              |> sync)

type database = Value.database
let runtime_error str = (Errors.runtime_error str)

class virtual db_args from_str = object
  val strval : string = from_str
  method virtual from_string : string -> unit
end

let get_variant_type t =
  match t with
    | Types.Variant (row) ->
                   begin
                    match row with
                      | Types.Row (x, _, _) ->
                                      let present_t = (StringMap.find "Just" x) in
                                        begin
                                        match present_t with
                                          | Types.Present actual_t -> actual_t
                                          | _ -> t
                                        end
                      | _ -> t
                   end
    | _ -> t



let rec value_of_db_string (value:string) t =
  let _ = Debug.print ("Right, in vdbs: " ^ value) in
  match TypeUtils.concrete_type t with
    | Types.Primitive Primitive.Bool ->
        (* HACK:

           This should probably be part of the database driver as
           different databases have different representations of
           booleans.

           mysql appears to use 0/1 and postgres f/t
        *)
        Value.box_bool (value = "1" || value = "t" || value = "true")
    | Types.Primitive Primitive.Char -> Value.box_char (String.get value 0)
    | Types.Primitive Primitive.String -> Value.box_string value
    | Types.Primitive Primitive.Int  ->
        (* HACK: Currently Links does not properly handle integers
         * if they are null. This is a temporary workaround (hack) to
         * allow us to at least interface with DBs containing nulls,
         * until we manage to do the research required to do something
         * more principled.
         * If "coerce_null_integers" is true and a null integer is found,
         * then instead of crashing, "null_integer" is used instead. *)
        if value = "" then
          if Settings.get coerce_null_integers then
            Value.box_int (val_of (Settings.get null_integer))
          else
            raise (Errors.RuntimeError ("Attempted to read null integer from the database"))
        else
          Value.box_int (int_of_string value)
    | Types.Primitive Primitive.Float ->
       if value = "" then Value.box_float 0.00      (* HACK HACK *)
       else Value.box_float (float_of_string value)
    | Types.Primitive Primitive.DateTime ->
       Value.box_datetime (Timestamp.parse_db_string value)
    | Types.Variant _ -> let _ = ("Here is the value, here it is: " ^ value) in
                        if (String.equal value "") then
                                Value.box_variant "Nothing" (Value.box_string "")
                        else
                                Value.box_variant "Just" (value_of_db_string value (get_variant_type t)) 
    | t -> raise (runtime_error
      ("value_of_db_string: unsupported datatype: '" ^
        Types.string_of_datatype t ^"'"))

let execute_command  (query:string) (db: database) : Value.t =
  let result = (db#exec query) in
    begin
      match result#status with
        | `QueryOk -> `Record []
        | `QueryError msg ->
            raise (runtime_error
              ("An error occurred executing the query " ^ query ^ ": " ^ msg))
    end

let execute_insert_returning returning q db =
  let qs = db#make_insert_returning_query returning q in
  let rec run =
    function
      | [] -> assert false
      | [q] ->
          let result = db#exec q in
            begin
              match result#status with
               | `QueryOk ->
                  if result#nfields = 1 && result#ntuples = 1
                  then (* returning field has to be of type int *)
                    Value.box_int (int_of_string (result#getvalue 0 0))
                  else raise (runtime_error ("Returned the wrong number of results executing " ^ q))
               | `QueryError msg ->
                   raise (runtime_error ("An error occurred executing the query " ^ q ^ ": " ^ msg))
            end
      | q :: qs ->
        let _unit = execute_command q db in
        run qs
  in
    run qs


let is_null name = (name = "null")

let result_signature field_types result =
    let n = result#nfields in
    let _ = Debug.print ("Number of fields: " ^ string_of_int n) in
    let rec rs i =
      if i >= n then
        [],true
      else
        let name = result#fname i in
          if start_of ~is:"order_" name then
            (* ignore ordering fields *)
            rs (i+1)
          else if start_of ~is:"@unit@" name then
            let fields,null_query = rs (i+1) in
            (name, (Types.unit_type,i)) :: fields,
            null_query
          else if List.mem_assoc name field_types then
            let fields,null_query = rs (i+1) in
            let _ = Debug.print ("Hmmm " ^ ( Types.string_of_datatype (List.assoc name field_types))) in
            (name, (List.assoc name field_types, i)) :: fields,
            null_query && is_null(name)
          else
            raise (runtime_error
              ("Column " ^ name ^
               " had no type info in query's type spec: " ^
               mapstrcat ", " (fun (name, t) -> name ^ ":" ^
                 Types.string_of_datatype t)
               field_types))
    in let rs, null_query = rs 0
    in if null_query then [] else rs


(* builds record given a row field accessor function *)
 let build_record (rs: (string * (Types.datatype * int)) list) (row:int -> string) =
    let rec build rs l =
      match rs with
      | [] -> l
      | (name,(t,i))::rs' ->
                      Debug.print ("in build record: " ^ (Types.string_of_datatype t) );
                      Debug.print ("Also in build record: " ^ (row i));
      build rs' (((name),value_of_db_string (row i) t)::l)
    in build rs []


let execute_select_result
    (field_types:(string * Types.datatype) list) (query:string) (db: database)  =
  let _ = Debug.print ("Running query: \n" ^ query) in
  let result = (db#exec query) in
    (match result#status with
       | `QueryOk ->
           result,
       result_signature field_types result
       | `QueryError msg ->
           raise (runtime_error
             ("An error occurred executing the query " ^ query ^ ": " ^ msg)))


let build_result ((result:Value.dbvalue),rs) =
  `List (result#map (fun row ->
                     `Record (build_record rs row))
       )

let execute_select
    (field_types:(string * Types.datatype) list) (query:string) (db : database)
    : Value.t =
  let result,rs = execute_select_result field_types query db in
  build_result (result,rs)

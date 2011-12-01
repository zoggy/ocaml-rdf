(** *)

open Rdf_types;;

(**/**)
module Raw =
  struct
    external new_storage : world ->
      string -> string -> string -> storage option = "ml_librdf_new_storage"

    external new_storage_from_options : world ->
      string -> string -> hash -> storage option = "ml_librdf_new_storage_from_options"

    external new_storage_from_storage : storage -> storage option =
      "ml_librdf_new_storage_from_storage"

    external free_storage : storage -> unit = "ml_librdf_free_storage"

    external new_storage_from_factory : world ->
      storage_factory -> string -> hash -> storage option =
      "ml_librdf_new_storage_from_factory"

    external storage_open : storage -> model -> bool =
      "ml_librdf_storage_open"
    external storage_close : storage -> bool =
      "ml_librdf_storage_close"


    external storage_size : storage -> int =
      "ml_librdf_storage_size"

    external storage_add_statement : storage -> statement -> int =
      "ml_librdf_storage_add_statement"

    external storage_add_statements : storage -> statement stream -> bool =
      "ml_librdf_storage_add_statements"

    external storage_remove_statement : storage -> statement -> bool =
      "ml_librdf_storage_remove_statement"

    external storage_contains_statement : storage -> statement -> int =
      "ml_librdf_storage_contains_statement"

    external storage_serialize : storage -> statement stream option =
      "ml_librdf_storage_serialize"

    external pointer_of_storage : storage -> Nativeint.t = "ml_pointer_of_custom"
   end

let storage_to_finalise v = Gc.finalise Raw.free_storage v;;
(**/**)

exception Storage_creation_failed of string;;
exception Illegal_statement

let on_new_storage fun_name = function
  None -> raise (Storage_creation_failed fun_name)
| Some n -> storage_to_finalise n; n
;;

let new_storage ?(options="") world ~factory ~name =
  on_new_storage "" (Raw.new_storage world factory name options)
;;

let new_storage_from_options world ~factory ~name hash =
 on_new_storage "from_options" (Raw.new_storage_from_options world factory name hash)
;;

let copy_storage storage =
  on_new_storage "from_storage" (Raw.new_storage_from_storage storage)
;;

let new_storage_from_factory world factory ~name hash =
 on_new_storage "from_factory" (Raw.new_storage_from_factory world factory name hash)
;;

let storage_open storage model =
  if not (Raw.storage_open storage model) then
    failwith "storage_open"
;;

let storage_close storage =
  if not (Raw.storage_close storage) then
    failwith "storage_close"
;;

let storage_size storage =
  let n = Raw.storage_size storage in
  if n < 0 then None else Some n
;;

let storage_add_statement storage statement =
  let n = Raw.storage_add_statement storage statement in
  if n < 0 then failwith "storage_add_statement";
  if n > 0 then raise Illegal_statement
;;

let storage_add_statements storage stream =
  if not (Raw.storage_add_statements storage stream) then
   failwith "storage_add_statements"
;;

let storage_remove_statement storage statement =
  if not (Raw.storage_remove_statement storage statement) then
   failwith "storage_remove_statement"
;;

let storage_contains_statement storage statement =
  let n = Raw.storage_contains_statement storage statement in
  if n > 0 then raise Illegal_statement;
  (n = 0)
;;

let storage_serialize storage =
  Rdf_stream.on_new_stream "storage_serialize" (Raw.storage_serialize storage)
;;
  
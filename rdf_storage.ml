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

    external storage_add_statements : storage -> statement stream -> int =
      "ml_librdf_storage_add_statements"

    external storage_remove_statement : storage -> statement -> int =
      "ml_librdf_storage_remove_statement"

    external storage_contains_statement : storage -> statement -> int =
      "ml_librdf_storage_contains_statement"

    external storage_serialise : storage -> statement stream option =
      "ml_librdf_storage_serialise"

    external storage_find_statements : storage -> statement -> statement stream option =
      "ml_librdf_storage_find_statements"

    external storage_find_statements_with_options :
      storage -> statement -> node option -> hash option -> statement stream option =
      "ml_librdf_storage_find_statements_with_options"

    external storage_get_sources : storage -> node -> node -> node iterator option =
      "ml_librdf_storage_get_sources"
    external storage_get_arcs : storage -> node -> node -> node iterator option =
      "ml_librdf_storage_get_arcs"
    external storage_get_targets : storage -> node -> node -> node iterator option =
      "ml_librdf_storage_get_targets"

    external storage_get_arcs_in : storage -> node -> node iterator option =
      "ml_librdf_storage_get_arcs_in"
    external storage_get_arcs_out : storage -> node -> node iterator option =
      "ml_librdf_storage_get_arcs_out"

    external storage_has_arc_in : storage -> node -> node -> bool =
      "ml_librdf_storage_hash_arc_in"
    external storage_has_arc_out : storage -> node -> node -> bool =
      "ml_librdf_storage_hash_arc_out"

    external storage_context_add_statement : storage -> node -> statement -> int =
      "ml_librdf_storage_context_add_statement"
    external storage_context_add_statements : storage -> node -> statement stream -> int =
      "ml_librdf_storage_context_add_statements"
    external storage_context_remove_statement : storage -> node -> statement -> int =
      "ml_librdf_storage_context_remove_statement"

    external storage_context_as_stream : storage -> node -> statement stream option =
      "ml_librdf_storage_context_as_stream"

    external storage_supports_query : storage -> query -> bool =
      "ml_librdf_storage_supports_query"

    external storage_query_execute : storage -> query -> query_results option =
      "ml_librdf_storage_query_execute"

    external storage_sync : storage -> int = "ml_librdf_storage_sync"

    external storage_find_statements_in_context :
      storage -> statement -> node option -> statement stream option =
      "ml_librdf_storage_find_statements_in_context"

    external storage_get_contexts : storage -> node iterator option =
      "ml_librdf_storage_get_contexts"

    external storage_get_feature : storage -> uri -> node option =
      "ml_librdf_storage_get_feature"

    external storage_set_feature : storage -> uri -> node -> int =
      "ml_librdf_storage_set_feature"

    external storage_transaction_commit : storage -> int = "ml_librdf_storage_transaction_commit"
    external storage_transaction_get_handle : storage -> 'a = "ml_librdf_storage_transaction_get_handle"
    external storage_transaction_rollback : storage -> int = "ml_librdf_storage_transaction_rollback"
    external storage_transaction_start : storage -> int = "ml_librdf_storage_transaction_start"
    external storage_transaction_start_with_handle :
      storage -> 'a -> int = "ml_librdf_storage_transaction_start_with_handle"

    external storage_get_world : storage -> world = "ml_librdf_storage_get_world"

    external pointer_of_storage : storage -> Nativeint.t = "ml_pointer_of_custom"
   end

let storage_to_finalise v = Gc.finalise Raw.free_storage v;;
(**/**)

exception Storage_creation_failed of string;;
exception Illegal_statement
exception No_such_feature of uri;;

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

let storage_add_statement storage ?context statement =
  let n =
    match context with
      None -> Raw.storage_add_statement storage statement
    | Some node -> Raw.storage_context_add_statement storage node statement
  in
  if n < 0 then failwith "storage_add_statement";
  if n > 0 then raise Illegal_statement
;;

let storage_add_statements storage ?context stream =
  let n =
    match context with
      None -> Raw.storage_add_statements storage stream
    | Some node -> Raw.storage_context_add_statements storage node stream
  in
  if n <> 0 then
   failwith "storage_add_statements"
;;

let storage_remove_statement storage ?context statement =
  let n =
    match context with
     None -> Raw.storage_remove_statement storage statement
   | Some node -> Raw.storage_context_remove_statement storage node statement
  in
  if n <> 0 then
   failwith "storage_remove_statement"
;;

let storage_contains_statement storage statement =
  let n = Raw.storage_contains_statement storage statement in
  if n > 0 then raise Illegal_statement;
  (n = 0)
;;

let storage_serialise ?context storage =
  let s =
    match context with
      None -> Raw.storage_serialise storage
    | Some node -> Raw.storage_context_as_stream storage node
  in
  Rdf_stream.on_new_stream "storage_serialise" s
;;

let storage_find_statements storage ?context ?hash statement =
  match context, hash with
    Some _, None ->
      Rdf_stream.on_new_stream "storage_find_statements_in_context"
        (Raw.storage_find_statements_in_context storage statement context)
  | None, None ->
      Rdf_stream.on_new_stream "storage_find_statements"
      (Raw.storage_find_statements storage statement)
  | _ ->
      Rdf_stream.on_new_stream "storage_find_statements_with_options"
         (Raw.storage_find_statements_with_options
            storage statement context hash)
;;

let storage_get_sources storage ~arc ~target =
  Rdf_iterator.on_new_iterator "storage_get_sources"
    (Raw.storage_get_sources storage arc target)
;;

let storage_get_arcs storage ~source ~target =
  Rdf_iterator.on_new_iterator "storage_get_arcs"
    (Raw.storage_get_arcs storage source target)
;;

let storage_get_targets storage ~source ~arc =
  Rdf_iterator.on_new_iterator "storage_get_targets"
    (Raw.storage_get_targets storage source arc)
;;

let storage_get_arcs_in storage node =
  Rdf_iterator.on_new_iterator "storage_get_arcs_in"
    (Raw.storage_get_arcs_in storage node)
;;

let storage_get_arcs_out storage node =
  Rdf_iterator.on_new_iterator "storage_get_arcs_out"
    (Raw.storage_get_arcs_out storage node)
;;

let storage_has_arc_in storage ~node ~property =
  Raw.storage_has_arc_in storage node property;;

let storage_has_arc_out storage ~node ~property =
  Raw.storage_has_arc_in storage node property;;

let storage_supports_query = Raw.storage_supports_query;;

let storage_query_execute storage query =
  Rdf_query_results.on_new_query_results "storage_query_execute"
    (Raw.storage_query_execute storage query)
;;

let storage_sync storage =
  if Raw.storage_sync storage <> 0 then failwith "storage_sync"
;;

let storage_get_contexts storage =
  Rdf_iterator.on_new_iterator "storage_get_contexts"
    (Raw.storage_get_contexts storage)
;;

let storage_get_feature storage uri =
  match Raw.storage_get_feature storage uri with
    None -> None
  | n -> Some (Rdf_node.on_new_node "" n)
;;

let storage_set_feature storage uri value =
  let n = Raw.storage_set_feature storage uri value in
  if n < 0 then raise (No_such_feature uri);
  if n > 0 then failwith "storage_set_feature"
;;

let storage_transaction_commit storage =
  let n = Raw.storage_transaction_commit storage in
  if n <> 0 then failwith "storage_transaction_commit"
;;

let storage_transaction_get_handle storage =
  Raw.storage_transaction_get_handle storage
;;

let storage_transaction_rollback storage =
  let n = Raw.storage_transaction_rollback storage in
  if n <> 0 then failwith "storage_transaction_rollback"
;;

let storage_transaction_start storage =
  let n = Raw.storage_transaction_start storage in
  if n <> 0 then failwith "storage_transaction_start"
;;

let storage_transaction_start_with_handle storage h =
  let n = Raw.storage_transaction_start_with_handle storage h in
  if n <> 0 then failwith "storage_transaction_start_with_handle"
;;

let storage_get_world storage =
  Rdf_init.on_new_world "storage_get_world" (Some (Raw.storage_get_world storage))
;;

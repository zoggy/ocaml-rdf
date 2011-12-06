(** *)

open Rdf_types

let dbg = Rdf_misc.create_log_fun ~prefix: "Rdf_model" "ORDF_MODEL";;

(**/**)
module Raw =
  struct
    external new_model : world -> storage -> string -> model option = "ml_librdf_new_model"
    external free : model -> unit = "ml_librdf_free_model"
    external new_from_model : model -> model = "ml_librdf_new_model_from_model"

    external add_statement : model -> statement -> int =
      "ml_librdf_model_add_statement"

    external find_statements : model -> statement -> statement stream option =
      "ml_librdf_model_find_statements"

    external get_sources : model -> node -> node -> node iterator option =
      "ml_librdf_model_get_sources"
    external get_arcs : model -> node -> node -> node iterator option =
      "ml_librdf_model_get_arcs"
    external get_targets : model -> node -> node -> node iterator option =
      "ml_librdf_model_get_targets"

    external write : model -> raptor_iostream -> int =
      "ml_librdf_model_write"

    external pointer_of_model : model -> Nativeint.t = "ml_pointer_of_custom"
end

let free v =
  dbg (fun () -> Printf.sprintf "Freeing model %s"
   (Nativeint.to_string (Raw.pointer_of_model v)));
  Raw.free v
;;

let to_finalise v = Gc.finalise free v;;
(**/**)

exception Model_creation_failed of string;;

let on_new_model fun_name = function
  None -> raise (Model_creation_failed fun_name)
| Some n -> to_finalise n; n
;;

let new_model ?(options="") world storage =
  on_new_model "" (Raw.new_model world storage options)
;;

let add_statement model statement =
  let n = Raw.add_statement model statement in
  if n <> 0 then failwith "model_add_statement"
;;

let find_statements model ?context ?hash statement =
  match context, hash with
    Some _, None ->
      assert false (*Rdf_stream.on_new_stream "model_find_statements_in_context"
        (Raw.find_statements_in_context model statement context)*)
  | None, None ->
      Rdf_stream.on_new_stream "model_find_statements"
      (Raw.find_statements model statement)
  | _ ->
      assert false
      (*Rdf_stream.on_new_stream "model_find_statements_with_options"
         (Raw.find_statements_with_options
            model statement context hash)*)
;;

let get_sources model ~arc ~target =
  Rdf_iterator.on_new_iterator "model_get_sources"
    (Raw.get_sources model arc target)
;;

let get_arcs model ~source ~target =
  Rdf_iterator.on_new_iterator "model_get_arcs"
    (Raw.get_arcs model source target)
;;

let get_targets model ~source ~arc =
  Rdf_iterator.on_new_iterator "model_get_targets"
    (Raw.get_targets model source arc)
;;

let write model iostream =
  let n = Raw.write model iostream in
  if n <> 0 then failwith "model_write"
;;
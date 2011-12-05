(** *)

open Rdf_types

(**/**)
module Raw =
  struct
    external new_model : world -> storage -> string -> model option = "ml_librdf_new_model"
    external free_model : model -> unit = "ml_librdf_free_model"
    external new_model_from_model : model -> model = "ml_librdf_new_model_from_model"

    external model_add_statement : model -> statement -> int =
      "ml_librdf_model_add_statement"

    external model_find_statements : model -> statement -> statement stream option =
      "ml_librdf_model_find_statements"

    external model_write : model -> raptor_iostream -> int =
      "ml_librdf_model_write"

    external pointer_of_model : model -> Nativeint.t = "ml_pointer_of_custom"
end

let model_to_finalise v = Gc.finalise Raw.free_model v;;
(**/**)

exception Model_creation_failed of string;;

let on_new_model fun_name = function
  None -> raise (Model_creation_failed fun_name)
| Some n -> model_to_finalise n; n
;;

let new_model ?(options="") world storage =
  on_new_model "" (Raw.new_model world storage options)
;;

let model_add_statement model statement =
  let n = Raw.model_add_statement model statement in
  if n <> 0 then failwith "model_add_statement"
;;

let model_find_statements model ?context ?hash statement =
  match context, hash with
    Some _, None ->
      assert false (*Rdf_stream.on_new_stream "model_find_statements_in_context"
        (Raw.model_find_statements_in_context model statement context)*)
  | None, None ->
      Rdf_stream.on_new_stream "model_find_statements"
      (Raw.model_find_statements model statement)
  | _ ->
      assert false
      (*Rdf_stream.on_new_stream "model_find_statements_with_options"
         (Raw.model_find_statements_with_options
            model statement context hash)*)
;;

let model_write model iostream =
  let n = Raw.model_write model iostream in
  if n <> 0 then failwith "model_write"
;;
(** *)

type model

external new_model : Rdf_init.world -> Rdf_storage.storage -> string -> model = "ml_librdf_new_model"
external free_model : model -> unit = "ml_librdf_free_model"
external new_model_from_model : model -> model = "ml_librdf_new_model_from_model"

external pointer_of_model : model -> Nativeint.t = "ml_pointer_of_custom"

(** @todo decr raptor counter when freeing world *)

let (add_model, incr_model, decr_model) =
  Rdf_misc.create_pointer_counter "model"
  pointer_of_model free_model;;

let new_model ?(options="") world storage =
  let m = new_model world storage options in
  add_model m;
  m
;;


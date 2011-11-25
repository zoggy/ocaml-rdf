(** *)

open Rdf_types

(**/**)
module Raw =
  struct
    external new_model : world -> storage -> string -> model option = "ml_librdf_new_model"
    external free_model : model -> unit = "ml_librdf_free_model"
    external new_model_from_model : model -> model = "ml_librdf_new_model_from_model"

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


(** *)

open Rdf_types;;

(**/**)
module Raw =
  struct
    external free_iterator : 'a iterator -> unit = "ml_librdf_free_iterator"
    external pointer_of_iterator : 'a iterator -> Nativeint.t = "ml_pointer_of_custom"
   end

let iterator_to_finalise v = Gc.finalise Raw.free_iterator v;;
(**/**)

exception Iterator_creation_failed of string;;

let on_new_iterator fun_name = function
  None -> raise (Iterator_creation_failed fun_name)
| Some n -> iterator_to_finalise n; n
;;


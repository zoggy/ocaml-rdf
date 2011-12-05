(** *)

open Rdf_types;;

(**/**)
module Raw =
  struct
    external free_iterator : 'a iterator -> unit = "ml_librdf_free_iterator"
    external iterator_end : 'a iterator -> bool = "ml_librdf_iterator_end"
    external iterator_next : 'a iterator -> bool = "ml_librdf_iterator_next"
    external iterator_get_object : 'a iterator -> 'a option = "ml_librdf_iterator_get_object"
    external iterator_get_context : 'a iterator -> 'b option = "ml_librdf_iterator_get_context"
(*
    external iterator_get_key : 'a iterator -> 'a option = "ml_librdf_iterator_get_key"
    external iterator_get_value : 'a iterator -> 'a option = "ml_librdf_iterator_get_value"
*)
    external pointer_of_iterator : 'a iterator -> Nativeint.t = "ml_pointer_of_custom"
   end

let iterator_to_finalise v = Gc.finalise Raw.free_iterator v;;
(**/**)

exception Iterator_creation_failed of string;;

let on_new_iterator fun_name = function
  None -> raise (Iterator_creation_failed fun_name)
| Some n -> iterator_to_finalise n; n
;;

let iterator_end = Raw.iterator_end ;;
let iterator_next = Raw.iterator_next ;;
let iterator_get_object it copy =
  Rdf_misc.map_opt copy (Raw.iterator_get_object it);;
let iterator_get_context ?(copy=(fun x -> x)) it =
  Rdf_misc.map_opt copy (Raw.iterator_get_context it)
;;


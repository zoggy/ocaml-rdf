(** Iterators.
  @rdfmod redland-iterator.html
  @rdfprefix librdf_
*)

open Rdf_types;;

(**/**)
let dbg = Rdf_misc.create_log_fun ~prefix: "Rdf_iterator" "ORDF_ITERATOR";;

module Raw =
  struct
    external free : 'a iterator -> unit = "ml_librdf_free_iterator"
    external is_at_end : 'a iterator -> bool = "ml_librdf_iterator_end"
    external next : 'a iterator -> bool = "ml_librdf_iterator_next"
    external get_object : 'a iterator -> 'a option = "ml_librdf_iterator_get_object"
    external get_context : 'a iterator -> 'b option = "ml_librdf_iterator_get_context"
(*
    external get_key : 'a iterator -> 'a option = "ml_librdf_iterator_get_key"
    external get_value : 'a iterator -> 'a option = "ml_librdf_iterator_get_value"
*)
    external pointer_of_iterator : 'a iterator -> Nativeint.t = "ml_pointer_of_custom"
   end
let free v =
  dbg (fun () -> Printf.sprintf "Freeing iterator %s"
   (Nativeint.to_string (Raw.pointer_of_iterator v)));
  Raw.free v
;;
let to_finalise v = Gc.finalise free v;;
(**/**)

exception Iterator_creation_failed of string;;

let on_new_iterator fun_name = function
  None -> raise (Iterator_creation_failed fun_name)
| Some n -> to_finalise n; n
;;

(** @rdf iterator_end *)
let is_at_end = Raw.is_at_end ;;

(** @rdf iterator_next *)
let next = Raw.next ;;

(** @rdf iterator_get_object *)
let get_object it copy =
  Rdf_misc.map_opt copy (Raw.get_object it);;

(** @rdf iterator_get_context *)
let get_context ?(copy=(fun x -> x)) it =
  Rdf_misc.map_opt copy (Raw.get_context it)
;;


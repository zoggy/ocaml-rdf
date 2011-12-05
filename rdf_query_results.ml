(** *)

open Rdf_types;;

(**/**)
module Raw =
  struct
    external free_query_results : query_results -> unit = "ml_librdf_free_query_results"

    external pointer_of_query : query -> Nativeint.t = "ml_pointer_of_custom"
   end

let query_results_to_finalise v = Gc.finalise Raw.free_query_results v;;
(**/**)

exception Query_results_creation_failed of string;;

let on_new_query_results fun_name = function
  None -> raise (Query_results_creation_failed fun_name)
| Some n -> query_results_to_finalise n; n
;;


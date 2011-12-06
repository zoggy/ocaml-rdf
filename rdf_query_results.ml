(** *)

open Rdf_types;;

let dbg = Rdf_misc.create_log_fun ~prefix: "Rdf_query_results" "ORDF_QUERY_RESULTS";;

(**/**)
module Raw =
  struct
    external free : query_results -> unit = "ml_librdf_free_query_results"

    external pointer_of_query_results : query_results -> Nativeint.t = "ml_pointer_of_custom"
   end

let free v =
  dbg (fun () -> Printf.sprintf "Freeing query results %s"
   (Nativeint.to_string (Raw.pointer_of_query_results v)));
  Raw.free v
;;
let to_finalise v = Gc.finalise free v;;
(**/**)

exception Query_results_creation_failed of string;;

let on_new_query_results fun_name = function
  None -> raise (Query_results_creation_failed fun_name)
| Some n -> to_finalise n; n
;;


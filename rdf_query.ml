(** *)

open Rdf_types;;

let dbg = Rdf_misc.create_log_fun ~prefix: "Rdf_query" "ORDF_QUERY";;

(**/**)
module Raw =
  struct
    external new_query : world ->
      string -> uri option -> string -> uri option -> query option = "ml_librdf_new_query"

    external new_from_query :
      query -> query option = "ml_librdf_new_query_from_query"
    external free : query -> unit = "ml_librdf_free_query"

    external pointer_of_query : query -> Nativeint.t = "ml_pointer_of_custom"
   end

let free v =
  dbg (fun () -> Printf.sprintf "Freeing query %s"
   (Nativeint.to_string (Raw.pointer_of_query v)));
  Raw.free v
;;
let to_finalise v = Gc.finalise free v;;
(**/**)

exception Query_creation_failed of string;;

let on_new_query fun_name = function
  None -> raise (Query_creation_failed fun_name)
| Some n -> to_finalise n; n
;;

let new_query world ~name ?uri ?base ~query =
  on_new_query "" (Raw.new_query world name uri query base)
;;

let copy_query query =
  on_new_query "from_query" (Raw.new_from_query query)
;;

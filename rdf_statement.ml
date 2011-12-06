(** *)

open Rdf_types;;

let dbg = Rdf_misc.create_log_fun ~prefix: "Rdf_statement" "ORDF_STATEMENT";;

(**/**)
module Raw =
  struct
    external new_statement : world -> statement option = "ml_librdf_new_statement"
    external free : statement -> unit = "ml_librdf_free_statement"
    external new_from_statement : statement -> statement option = "ml_librdf_new_statement_from_statement"

    external new_from_nodes :
      world -> node -> node -> node -> statement option = "ml_librdf_new_statement_from_nodes"

    external init : world -> statement -> unit = "ml_librdf_statement_init"
    external clear : statement -> unit = "ml_librdf_statement_clear"

    external get_subject : statement -> node = "ml_librdf_statement_get_subject"
    external set_subject : statement -> node -> unit = "ml_librdf_statement_set_subject"

    external get_predicate : statement -> node = "ml_librdf_statement_get_predicate"
    external set_predicate : statement -> node -> unit = "ml_librdf_statement_set_predicate"

    external get_object : statement -> node = "ml_librdf_statement_get_object"
    external set_object : statement -> node -> unit = "ml_librdf_statement_set_object"

    external is_complete : statement -> bool = "ml_librdf_statement_is_complete"
    external equals : statement -> statement -> bool = "ml_librdf_statement_equals"
    external matches : statement -> statement -> bool = "ml_librdf_statement_match"

    external print : statement -> Unix.file_descr -> unit =
      "ml_librdf_statement_print"

    external pointer_of_statement : statement -> Nativeint.t = "ml_pointer_of_custom"
end;;

let free v =
  dbg (fun () -> Printf.sprintf "Freeing statement %s"
   (Nativeint.to_string (Raw.pointer_of_statement v)));
  Raw.free v
;;
let to_finalise v = Gc.finalise free v;;
(**/**)

exception Statement_creation_failed of string;;

let on_new_statement fun_name = function
  None -> raise (Statement_creation_failed fun_name)
| Some n -> to_finalise n; n
;;

let new_statement world =
  on_new_statement "" (Raw.new_statement world)
;;
let copy_statement st =
  on_new_statement "from_statement" (Raw.new_from_statement st)
;;
let new_from_nodes world ~sub ~pred ~obj =
  on_new_statement "from_nodes"
  (Raw.new_from_nodes world sub pred obj)
;;
let get_subject st =
  Rdf_node.copy_node (Raw.get_subject st)
;;
let set_subject st n =
  Raw.set_subject st (Rdf_node.copy_node n)
;;

let get_predicate st =
  Rdf_node.copy_node (Raw.get_predicate st)
;;
let set_predicate st n =
  Raw.set_predicate st (Rdf_node.copy_node n)
;;

let get_object st =
  Rdf_node.copy_node (Raw.get_object st)
;;
let set_object st n =
  Raw.set_object st (Rdf_node.copy_node n)
;;
let print statement outch =
  Raw.print statement outch;;


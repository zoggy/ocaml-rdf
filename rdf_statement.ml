(** *)

open Rdf_types;;

(**/**)
module Raw =
  struct
    external new_statement : world -> statement option = "ml_librdf_new_statement"
    external free_statement : statement -> unit = "ml_librdf_free_statement"
    external new_statement_from_statement : statement -> statement option = "ml_librdf_new_statement_from_statement"

    external new_statement_from_nodes :
      world -> node -> node -> node -> statement option = "ml_librdf_new_statement_from_nodes"

    external statement_init : world -> statement -> unit = "ml_librdf_statement_init"
    external statement_clear : statement -> unit = "ml_librdf_statement_clear"

    external statement_get_subject : statement -> node = "ml_librdf_statement_get_subject"
    external statement_set_subject : statement -> node -> unit = "ml_librdf_statement_set_subject"

    external statement_get_predicate : statement -> node = "ml_librdf_statement_get_predicate"
    external statement_set_predicate : statement -> node -> unit = "ml_librdf_statement_set_predicate"

    external statement_get_object : statement -> node = "ml_librdf_statement_get_object"
    external statement_set_object : statement -> node -> unit = "ml_librdf_statement_set_object"

    external statemet_is_complete : statement -> bool = "ml_librdf_statement_is_complete"
    external statemet_equals : statement -> statement -> bool = "ml_librdf_statement_equals"
    external statemet_match : statement -> statement -> bool = "ml_librdf_statement_match"

    external pointer_of_statement : statement -> Nativeint.t = "ml_pointer_of_custom"
end;;

let statement_to_finalise v = Gc.finalise Raw.free_statement v;;
(**/**)

exception Statement_creation_failed of string;;

let on_new_statement fun_name = function
  None -> raise (Statement_creation_failed fun_name)
| Some n -> statement_to_finalise n; n
;;

let new_statement world =
  on_new_statement "" (Raw.new_statement world)
;;
let copy_statement st =
  on_new_statement "from_statement" (Raw.new_statement_from_statement st)
;;
let new_statement_from_nodes world ~sub ~pred ~obj =
  on_new_statement "from_nodes"
  (Raw.new_statement_from_nodes world sub pred obj)
;;
let statement_get_subject st =
  Rdf_node.copy_node (Raw.statement_get_subject st)
;;
let statement_set_subject st n =
  Raw.statement_set_subject st (Rdf_node.copy_node n)
;;

let statement_get_predicate st =
  Rdf_node.copy_node (Raw.statement_get_predicate st)
;;
let statement_set_predicate st n =
  Raw.statement_set_predicate st (Rdf_node.copy_node n)
;;

let statement_get_object st =
  Rdf_node.copy_node (Raw.statement_get_object st)
;;
let statement_set_object st n =
  Raw.statement_set_object st (Rdf_node.copy_node n)
;;


(** *)

open Rdf_types;;

external new_statement : world -> statement option = "ml_librdf_new_statement"
external free_statement : statement -> unit = "ml_librdf_free_statement"
external new_statement_from_statement : statement -> statement option = "ml_librdf_statement_from_statement"

external new_statement_from_nodes :
  world -> node -> node -> node -> statement option = "ml_librdf_statement_from_nodes"

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

let (add_statement, incr_statement, decr_statement) =
  Rdf_misc.create_pointer_counter "statement"
  pointer_of_statement free_statement;;

let new_statement ?(options="") world ~factory ~name =
  let m = new_statement world in
  Rdf_misc.do_opt add_statement m;
  m
;;

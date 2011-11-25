(** *)

open Rdf_types;;

external new_statement : world -> statement option = "ml_librdf_new_statement"
external free_statement : statement -> unit = "ml_librdf_free_statement"

external pointer_of_statement : statement -> Nativeint.t = "ml_pointer_of_custom"

let (add_statement, incr_statement, decr_statement) =
  Rdf_misc.create_pointer_counter "statement"
  pointer_of_statement free_statement;;

let new_statement ?(options="") world ~factory ~name =
  let m = new_statement world in
  Rdf_misc.do_opt add_statement m;
  m
;;

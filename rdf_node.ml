(** *)

type node

external new_node : Rdf_init.world -> node option = "ml_librdf_new_node"
external free_node : node -> unit = "ml_librdf_free_node"

external pointer_of_node : node -> Nativeint.t = "ml_pointer_of_custom"


let (add_node, incr_node, decr_node) =
  Rdf_misc.create_pointer_counter "node"
  pointer_of_node free_node;;

let new_node world =
  let m = new_node world in
  Rdf_misc.do_opt add_node m;
  m
;;


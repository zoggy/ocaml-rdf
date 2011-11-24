(** *)

type world

external new_world : unit -> world = "ml_librdf_new_world"
external free_world : world -> unit = "ml_librdf_free_world"
external world_open : world -> unit = "ml_librdf_world_open"
external world_set_rasqal : world -> Rdf_rasqal.world -> unit = "ml_librdf_world_set_rasqal"
external pointer_of_world : world -> Nativeint.t = "caml_copy_nativeint"

let incr_world =
  Rdf_misc.create_pointer_counter "world"
  pointer_of_world free_world;;

let new_world () =
  let w = new_world () in
  incr_world w;
  w
;;
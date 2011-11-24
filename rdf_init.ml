(** *)

type world

external new_world : unit -> world = "ml_librdf_new_world"
external free_world : world -> unit = "ml_librdf_free_world"
external world_open : world -> unit = "ml_librdf_world_open"
external world_set_rasqal : world -> Rdf_rasqal.world option -> unit = "ml_librdf_world_set_rasqal"
external world_get_rasqal : world -> Rdf_rasqal.world option = "ml_librdf_world_get_rasqal"
external world_init_mutex : world -> unit = "ml_librdf_world_init_mutex"
external world_set_digest : world -> string -> unit = "ml_librdf_world_set_digest"

external pointer_of_world : world -> Nativeint.t = "ml_pointer_of_custom"

(** @todo decr rasqal counter when freeing world *)


let free_world w =
  Rdf_misc.do_opt
  (fun r -> Rdf_rasqal.decr_world r ; world_set_rasqal w None)
  (world_get_rasqal w)
;;

let (add_world, incr_world, decr_world) =
  Rdf_misc.create_pointer_counter "world"
  pointer_of_world free_world;;

let new_world () =
  let w = new_world () in
  add_world w;
  w
;;

let world_set_rasqal w r =
  Rdf_misc.do_opt Rdf_rasqal.decr_world (world_get_rasqal w);
  Rdf_misc.do_opt Rdf_rasqal.incr_world r;
  world_set_rasqal w r
;;


let world_get_rasqal w =
  let r = world_get_rasqal w in
  Rdf_misc.do_opt Rdf_rasqal.add_world r;
  r
;;

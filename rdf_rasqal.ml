(** *)

type world

external new_world : unit -> world = "ml_rasqal_new_world"
external free_world : world -> unit = "ml_rasqal_free_world"
external world_open : world -> int = "ml_rasqal_world_open"
external world_set_raptor : world -> Rdf_raptor.world option -> unit = "ml_rasqal_world_set_raptor"
external world_get_raptor : world -> Rdf_raptor.world option = "ml_rasqal_world_get_raptor"
external pointer_of_world : world -> Nativeint.t = "ml_pointer_of_custom"

(** @todo decr raptor counter when freeing world *)


let free_world w =
  Rdf_misc.do_opt
  (fun r -> Rdf_raptor.decr_world r ; world_set_raptor w None)
  (world_get_raptor w)
;;

let (add_world, incr_world, decr_world) =
  Rdf_misc.create_pointer_counter "rasqal_world"
  pointer_of_world free_world;;

let new_world () =
  let w = new_world () in
  add_world w;
  w
;;

let world_set_raptor w r =
  Rdf_misc.do_opt Rdf_raptor.decr_world (world_get_raptor w);
  Rdf_misc.do_opt Rdf_raptor.incr_world r;
  world_set_raptor w r
;;

let world_get_raptor w =
  let r = world_get_raptor w in
  Rdf_misc.do_opt Rdf_raptor.add_world r;
  r
;;
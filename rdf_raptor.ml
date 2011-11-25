(** *)

open Rdf_types;;

external new_world : unit -> raptor_world = "ml_raptor_new_world"
external free_world : raptor_world -> unit = "ml_raptor_free_world"
external world_open : raptor_world -> int = "ml_raptor_world_open"
external pointer_of_world : raptor_world -> Nativeint.t = "ml_pointer_of_custom"

let (add_world, incr_world, decr_world) =
  Rdf_misc.create_pointer_counter "raptor_world"
  pointer_of_world free_world;;

let new_world () =
  let w = new_world () in
  add_world w;
  w
;;

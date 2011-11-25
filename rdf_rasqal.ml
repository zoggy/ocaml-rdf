(** *)

open Rdf_types;;

module Raw =
  struct
    external new_world : unit -> rasqal_world option = "ml_rasqal_new_world"
    external free_world : rasqal_world -> unit = "ml_rasqal_free_world"
    external world_open : rasqal_world -> int = "ml_rasqal_world_open"
    external world_set_raptor : rasqal_world -> raptor_world option -> unit = "ml_rasqal_world_set_raptor"
    external world_get_raptor : rasqal_world -> raptor_world option = "ml_rasqal_world_get_raptor"
    external pointer_of_world : rasqal_world -> Nativeint.t = "ml_pointer_of_custom"
  end

let world_to_finalise v =();;(* Gc.finalise Raw.free_world v;;*)

exception Rasqal_world_creation_failed of string;;

let on_new_world fun_name = function
  None -> raise (Rasqal_world_creation_failed fun_name)
| Some n -> world_to_finalise n; n
;;

let new_world () = on_new_world "" (Raw.new_world ());;
let world_open = Raw.world_open;;
let world_set_raptor = Raw.world_set_raptor;;
let world_get_raptor = Raw.world_get_raptor;;

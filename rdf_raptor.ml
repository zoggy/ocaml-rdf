(** *)

open Rdf_types;;

module Raw =
  struct
    external new_world : unit -> raptor_world option = "ml_raptor_new_world"
    external free_world : raptor_world -> unit = "ml_raptor_free_world"
    external world_open : raptor_world -> int = "ml_raptor_world_open"
    external pointer_of_world : raptor_world -> Nativeint.t = "ml_pointer_of_custom"
  end

let world_to_finalise v = () (*Gc.finalise Raw.free_world v;;*)

exception Raptor_world_creation_failed of string;;

let on_new_world fun_name = function
  None -> raise (Raptor_world_creation_failed fun_name)
| Some n -> world_to_finalise n; n
;;
let new_world () = on_new_world "" (Raw.new_world ());;

(** *)

open Rdf_types;;

(**/**)
module Raw =
  struct
    external new_world : unit -> world option = "ml_librdf_new_world"
    external free_world : world -> unit = "ml_librdf_free_world"
    external world_open : world -> unit = "ml_librdf_world_open"
    external world_set_rasqal : world -> rasqal_world option -> unit = "ml_librdf_world_set_rasqal"
    external world_get_rasqal : world -> rasqal_world option = "ml_librdf_world_get_rasqal"
    external world_init_mutex : world -> unit = "ml_librdf_world_init_mutex"
    external world_set_digest : world -> string -> unit = "ml_librdf_world_set_digest"

    external pointer_of_world : world -> Nativeint.t = "ml_pointer_of_custom"
end

let world_to_finalise v = () (*Gc.finalise Raw.free_world v;;*)
(**/**)

exception World_creation_failed of string;;

let on_new_world fun_name = function
  None -> raise (World_creation_failed fun_name)
| Some n -> world_to_finalise n; n
;;

let new_world () = on_new_world "" (Raw.new_world ());;
let world_open = Raw.world_open;;
let world_set_rasqal = Raw.world_set_rasqal;;
let world_get_rasqal = Raw.world_get_rasqal;;
let world_init_mutex = Raw.world_init_mutex;;
let world_set_digest = Raw.world_set_digest;;

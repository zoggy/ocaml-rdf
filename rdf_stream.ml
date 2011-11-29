(** *)

open Rdf_types;;

(**/**)
module Raw =
  struct
    external free_stream : 'a stream -> unit = "ml_librdf_free_stream"
    external pointer_of_stream : 'a stream -> Nativeint.t = "ml_pointer_of_custom"
   end

let stream_to_finalise v = Gc.finalise Raw.free_stream v;;
(**/**)

exception Stream_creation_failed of string;;

let on_new_stream fun_name = function
  None -> raise (Stream_creation_failed fun_name)
| Some n -> stream_to_finalise n; n
;;

(*
let new_hash world name =
  on_new_hash "" (Raw.new_hash world name)
;;

let copy_hash hash =
  on_new_hash "from_hash" (Raw.new_hash_from_hash hash)
;;
*)
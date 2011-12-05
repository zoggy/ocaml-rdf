(** *)

open Rdf_types;;

(**/**)
module Raw =
  struct
    external free_stream : 'a stream -> unit = "ml_librdf_free_stream"
    external stream_end : 'a stream -> bool = "ml_librdf_stream_end"
    external stream_next : 'a stream -> bool = "ml_librdf_stream_next"
    external stream_get_object : 'a stream -> statement option = "ml_librdf_stream_get_object"
    external stream_get_context2 : 'a stream -> node option = "ml_librdf_stream_get_context2"

    external pointer_of_stream : 'a stream -> Nativeint.t = "ml_pointer_of_custom"
   end

let stream_to_finalise v = Gc.finalise Raw.free_stream v;;
(**/**)

exception Stream_creation_failed of string;;

let on_new_stream fun_name = function
  None -> raise (Stream_creation_failed fun_name)
| Some n -> stream_to_finalise n; n
;;

let stream_end = Raw.stream_end;;
let stream_next = Raw.stream_next;;
let stream_get_object str =
  Rdf_misc.map_opt Rdf_statement.copy_statement (Raw.stream_get_object str)
;;
let stream_get_context2 str =
  Rdf_misc.map_opt Rdf_node.copy_node (Raw.stream_get_context2 str)
;;

(*
let new_stream world name =
  on_new_stream "" (Raw.new_stream world name)
;;

let copy_stream stream =
  on_new_stream "from_stream" (Raw.new_stream_from_stream stream)
;;
*)
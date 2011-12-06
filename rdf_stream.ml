(** *)

open Rdf_types;;

let dbg = Rdf_misc.create_log_fun ~prefix: "Rdf_stream" "ORDF_STREAM";;

(**/**)
module Raw =
  struct
    external free : 'a stream -> unit = "ml_librdf_free_stream"
    external is_at_end : 'a stream -> bool = "ml_librdf_stream_end"
    external next : 'a stream -> bool = "ml_librdf_stream_next"
    external get_object : 'a stream -> statement option = "ml_librdf_stream_get_object"
    external get_context2 : 'a stream -> node option = "ml_librdf_stream_get_context2"

    external pointer_of_stream : 'a stream -> Nativeint.t = "ml_pointer_of_custom"
   end

let free v =
  dbg (fun () -> Printf.sprintf "Freeing stream %s"
   (Nativeint.to_string (Raw.pointer_of_stream v)));
  Raw.free v
;;

let to_finalise v = Gc.finalise free v;;
(**/**)

exception Stream_creation_failed of string;;

let on_new_stream fun_name = function
  None -> raise (Stream_creation_failed fun_name)
| Some n -> to_finalise n; n
;;

let is_at_end = Raw.is_at_end;;
let next = Raw.next;;
let get_object str =
  Rdf_misc.map_opt Rdf_statement.copy_statement (Raw.get_object str)
;;
let get_context2 str =
  Rdf_misc.map_opt Rdf_node.copy_node (Raw.get_context2 str)
;;

(*
let new_stream world name =
  on_new_stream "" (Raw.new_stream world name)
;;

let copy_stream stream =
  on_new_stream "from_stream" (Raw.new_from_stream stream)
;;
*)
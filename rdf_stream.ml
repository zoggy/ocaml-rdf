(*********************************************************************************)
(*                OCaml-RDF                                                      *)
(*                                                                               *)
(*    Copyright (C) 2011 Institut National de Recherche en Informatique          *)
(*    et en Automatique. All rights reserved.                                    *)
(*                                                                               *)
(*    This program is free software; you can redistribute it and/or modify       *)
(*    it under the terms of the GNU Lesser General Public License version        *)
(*    2.1 or later as published by the Free Software Foundation.                 *)
(*                                                                               *)
(*    This program is distributed in the hope that it will be useful,            *)
(*    but WITHOUT ANY WARRANTY; without even the implied warranty of             *)
(*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the              *)
(*    GNU Library General Public License for more details.                       *)
(*                                                                               *)
(*    You should have received a copy of the GNU Lesser General Public           *)
(*    License along with this program; if not, write to the Free Software        *)
(*    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA                   *)
(*    02111-1307  USA                                                            *)
(*                                                                               *)
(*    Contact: Maxence.Guesdon@inria.fr                                          *)
(*                                                                               *)
(*                                                                               *)
(*********************************************************************************)

(** Streams.
@rdfmod redland-stream.html
@rdfprefix librdf_
*)

open Rdf_types;;

(**/**)
let dbg = Rdf_misc.create_log_fun ~prefix: "Rdf_stream" "ORDF_STREAM";;

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

(** @rdf stream_end *)
let is_at_end = Raw.is_at_end;;

(** @rdf stream_next *)
let next = Raw.next;;

(** @rdf stream_get_object *)
let get_object str =
  Rdf_misc.map_opt Rdf_statement.copy_statement (Raw.get_object str)
;;

(** @rdf stream_get_context2 *)
let get_context2 str =
  Rdf_misc.map_opt Rdf_node.copy_node (Raw.get_context2 str)
;;


let iter f (stream : Rdf_types.statement Rdf_types.stream) =
  while not (is_at_end stream) do
    begin
      match get_object stream with
        None -> ()
      | Some statement -> f statement
    end;
    ignore(next stream);
  done
;;


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

(** Query results.
  @rdfmod redland-query-results.html
  @rdfprefix librdf_
*)

open Rdf_types;;

(**/**)
let dbg = Rdf_misc.create_log_fun ~prefix: "Rdf_query_results" "ORDF_QUERY_RESULTS";;

module Raw =
  struct
    external free : query_results -> unit = "ml_librdf_free_query_results"
    external as_stream : query_results -> statement stream option =
      "ml_librdf_query_results_as_stream"
    external get_count :query_results -> int = "ml_librdf_query_results_get_count"
    external next :query_results -> bool = "ml_librdf_query_results_next"
    external finished :query_results -> bool = "ml_librdf_query_results_finished"
    external get_bindings : query_results -> (string array * node option array) option =
      "ml_librdf_query_results_get_bindings"
    external pointer_of_query_results : query_results -> Nativeint.t = "ml_pointer_of_custom"
   end

let free v =
  dbg (fun () -> Printf.sprintf "Freeing query results %s"
   (Nativeint.to_string (Raw.pointer_of_query_results v)));
  Raw.free v
;;
let to_finalise v = Gc.finalise free v;;
(**/**)

exception Query_results_creation_failed of string;;

let on_new_query_results fun_name = function
  None -> raise (Query_results_creation_failed fun_name)
| Some n -> to_finalise n; n
;;

(** @rdf query_results_as_stream *)
let as_stream qr =
  Rdf_stream.on_new_stream "query_results_as_stream" (Raw.as_stream qr)
;;

(** @rdf query_results_ *)
let get_count = Raw.get_count ;;

(** @rdf query_results_next
  @return [true] in case of failure or results exhausted. *)
let next = Raw.next;;

(** @rdf query_results_finished
  @return [true] when results exhausted or query failed. *)
let finished = Raw.finished;;

(** @rdf query_results_get_bindings *)
let get_bindings qr =
  match Raw.get_bindings qr with
    None -> None
  | Some (names, nodes) ->
      dbg ~level: 2 (fun () -> "Rdf_query_results.get_bindings: back from C");
      (*let nodes = Array.map
       (function
           None -> None
         | Some n -> Some (Rdf_node.on_new_node "query_results_get_bindings" (Some n))
       )
       nodes
      in
      *)
      Some (names, nodes)
;;

(*
(** @rdf query_results_ *)
let qr =
;;

(** @rdf query_results_ *)
let qr =
;;

(** @rdf query_results_ *)
let qr =
;;

(** @rdf query_results_ *)
let qr =
;;

*)
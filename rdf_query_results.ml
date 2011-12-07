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
    external get_bindings_count :query_results -> int =
      "ml_librdf_query_results_get_bindings_count"
    external get_bindings : query_results -> (string array * node option array) option =
      "ml_librdf_query_results_get_bindings"
    external get_binding_value : query_results -> int -> node option =
      "ml_librdf_query_results_get_binding_value"
    external get_binding_name : query_results -> int -> string option =
      "ml_librdf_query_results_get_binding_name"
    external get_binding_value_by_name : query_results -> string -> node option =
      "ml_librdf_query_results_get_binding_value_by_name"
    external to_string2 : query_results ->
      string option -> string option -> uri option -> uri option -> string option =
       "ml_librdf_query_results_to_string2"
    external to_file_handle2 : query_results ->
      Unix.file_descr -> string option -> string option -> uri option -> uri option -> int =
       "ml_librdf_query_results_to_file_handle2_bc"
       "ml_librdf_query_results_to_file_handle2"

    external is_bindings : query_results -> bool = "ml_librdf_query_results_is_bindings"
    external is_boolean : query_results -> bool = "ml_librdf_query_results_is_boolean"
    external is_graph : query_results -> bool = "ml_librdf_query_results_is_graph"
    external is_syntax : query_results -> bool = "ml_librdf_query_results_is_syntax"
    external get_boolean : query_results -> int = "ml_librdf_query_results_get_boolean"

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

(** @rdf query_results_count *)
let get_count = Raw.get_count ;;

(** @rdf query_results_next
  @return [true] in case of failure or results exhausted. *)
let next = Raw.next;;

(** @rdf query_results_finished
  @return [true] when results exhausted or query failed. *)
let finished = Raw.finished;;

(** @rdf query_results_get_bindings_count *)
let get_bindings_count qr =
  let n = Raw.get_bindings_count qr in
  if n < 0 then None else Some n
;;

(** @rdf query_results_get_bindings *)
let get_bindings qr =
  match Raw.get_bindings qr with
    None -> None
  | Some (names, nodes) ->
      dbg ~level: 2 (fun () -> "Rdf_query_results.get_bindings: back from C");
      let nodes = Array.map
        (Rdf_node.on_new_node_opt "query_results_get_bindings")
        nodes
      in
      Some (names, nodes)
;;


(** @rdf query_results_get_binding_value *)
let get_binding_value qr n =
  Rdf_node.on_new_node_opt "query_results_get_binding_value"
    (Raw.get_binding_value qr n)
;;

(** @rdf query_results_get_binding_name *)
let get_binding_name = Raw.get_binding_name;;

(** @rdf query_results_get_binding_value_by_name *)
let get_binding_value_by_name qr name =
  Rdf_node.on_new_node_opt "query_results_get_binding_value_by_name"
    (Raw.get_binding_value_by_name qr name)
;;

(** @rdf query_results_to_string2 *)
let to_string2 ?name ?mimetype ?format ?base qr =
  Raw.to_string2 qr name mimetype format base
;;

(** @rdf query_results_to_file_handle2 *)
let to_file_handle2 qr ?name ?mimetype ?format ?base fd =
  let n = Raw.to_file_handle2 qr fd name mimetype format base in
  if n <> 0 then failwith "query_results_to_file_handle2"
;;

(** @rdf query_results_is_bindings *)
let is_bindings = Raw.is_bindings ;;

(** @rdf query_results_is_boolean *)
let is_boolean = Raw.is_boolean ;;

(** @rdf query_results_is_graph *)
let is_graph = Raw.is_graph ;;

(** @rdf query_results_is_syntax *)
let is_syntax = Raw.is_syntax ;;

(** @rdf query_results_get_boolean *)
let get_boolean qr =
  let n = Raw.get_boolean qr in
  if n < 0 then None else Some (n > 0)
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
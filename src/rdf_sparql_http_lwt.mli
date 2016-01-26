(*********************************************************************************)
(*                OCaml-RDF                                                      *)
(*                                                                               *)
(*    Copyright (C) 2012-2016 Institut National de Recherche en Informatique     *)
(*    et en Automatique. All rights reserved.                                    *)
(*                                                                               *)
(*    This program is free software; you can redistribute it and/or modify       *)
(*    it under the terms of the GNU Lesser General Public License version        *)
(*    3 as published by the Free Software Foundation.                            *)
(*                                                                               *)
(*    This program is distributed in the hope that it will be useful,            *)
(*    but WITHOUT ANY WARRANTY; without even the implied warranty of             *)
(*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the              *)
(*    GNU General Public License for more details.                               *)
(*                                                                               *)
(*    You should have received a copy of the GNU General Public License          *)
(*    along with this program; if not, write to the Free Software                *)
(*    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA                   *)
(*    02111-1307  USA                                                            *)
(*                                                                               *)
(*    Contact: Maxence.Guesdon@inria.fr                                          *)
(*                                                                               *)
(*********************************************************************************)

(** Lwt-based Sparql Protocol HTTP Binding.

   Using the {!Rdf_sparql_http.Make} functor.
*)


(** [base_headers ()] Gives the base headers used for bindings *)
val base_headers : ?accept: string -> unit -> Cohttp.Header.t

(** [result_of_response f response]
    [f] will be applied on the content-type and body string of the response.
    If the status is between 200 and 300 exclued, [f] is called,
    else {!Rdf_sparql_protocol}[.Error] is returned.
*)
val result_of_response:
  (content_type: string -> string -> Rdf_sparql_protocol.out_message) ->
    Cohttp.Response.t * Cohttp_lwt_body.t ->
    Rdf_sparql_protocol.out_message Lwt.t

(** The following items are created with the {!Rdf_sparql_http.Make} functor.
  See the documentation of {!Rdf_sparql_http.S}.
*)

type result = Rdf_sparql_protocol.out_message Lwt.t

val get : ?graph: Rdf_graph.graph -> base:Iri.t -> ?accept: string ->
  Uri.t -> Rdf_sparql_protocol.in_message -> result

val post : ?graph: Rdf_graph.graph -> base:Iri.t -> ?accept: string ->
  Uri.t -> ?query_var: string -> Rdf_sparql_protocol.in_message ->
  result

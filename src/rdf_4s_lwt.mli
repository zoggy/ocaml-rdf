(*********************************************************************************)
(*                OCaml-RDF                                                      *)
(*                                                                               *)
(*    Copyright (C) 2012-2014 Institut National de Recherche en Informatique     *)
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

(**
   {b Rdf 4Store -
   This Module implement the http binding of 4Store}
*)

open Rdf_sparql_http_lwt

(** {6 Bindings}
    In this document:
    [uri] is the server's uri (including the port)
    [query] is the SPARQL query
    [graph_uri] is the uri of the graph where the query is going to be executed.
    [graph_uri] if is equal to [uri] it is executed on the default graph
    [data_type] are set at "x-turtle" by default. *)

(** [get uri ?default_graph_uri ?named_graph_uri query]
    [query] If you could like to select a named-graph, it has to be made in the query.
    This method allows: select/ask/describe query.*)
val get : ?graph: Rdf_graph.graph -> base:Rdf_iri.iri -> ?accept: string ->
  Rdf_uri.uri -> Rdf_sparql_protocol.in_message ->
    Rdf_sparql_protocol.out_message Lwt.t

(** [post_update uri query]
    [query] If you would like to update a named-graph, it has to be made in the query.
    This method allows: update/insert/delete query. *)
val post_update : Rdf_uri.uri -> string -> Rdf_sparql_protocol.out_message Lwt.t

(** [delete uri graph_uri]
    This method removes the entire [graph_uri].*)
val delete : Rdf_uri.uri -> Rdf_uri.uri -> Rdf_sparql_protocol.out_message Lwt.t

(** [put uri data ?data_type graph_uri]
    This method allows to replace initial data from [graph_uri] by [data].
    [data] is allowed as rdf-xml type.*)
val put : Rdf_uri.uri -> string -> ?data_type:string -> Rdf_uri.uri ->
  Rdf_sparql_protocol.out_message Lwt.t

(** [post_append uri data ?data_type graph_uri]
    This method allow to append [data] into [graph_uri].
    [data] is allow as ttl type. *)
val post_append : Rdf_uri.uri -> string -> ?data_type:string -> Rdf_uri.uri ->
  Rdf_sparql_protocol.out_message Lwt.t

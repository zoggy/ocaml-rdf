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

(**
   {b Rdf 4Store -
   This Module implement the http binding of 4Store}
*)

open Rdf_sparql_http_lwt

(** {6 Bindings}
    The following get and post are using Rdf_sparql_http_lwt get en post.
    [graph_uri] is the uri of the graph where the query is going to be executed.
    [graph_uri] if is equal to [uri] it is executed on the default graph *)

(** [get url msg]
    [url] including "/sparql/"
    This method allows: select/ask/describe queries.*)
val get : ?graph: Rdf_graph.graph -> base:Iri.t -> ?accept: string ->
  Uri.t -> Rdf_sparql_protocol.in_message -> result

(** [post_update url msg]
    [url] including "/update/"
    This method allows: update/insert/delete queries. *)
val post_update : ?graph: Rdf_graph.graph -> base:Iri.t ->
  ?accept: string -> Uri.t -> Rdf_sparql_protocol.in_message -> result

(** [delete url graph_uri]
    [url] including "/data/"
    This method removes the entire [graph_uri]. *)
val delete : Uri.t -> Uri.t -> result

(** [put url content content_type graph_uri]
    [url] including "/data/"
    This method allows to replace initial data from [graph_uri] by [content].
    Refer to the 4Store documentation to know authorized content_type. *)
val put : Uri.t -> string -> string -> Uri.t -> result

(** [post_append url content content_type graph_uri]
    [url] including "/data/"
    This method allow to append [content] into [graph_uri].
    Refer to the 4Store documentation to know authorized content_type. *)
val post_append : Uri.t -> string -> string -> Uri.t -> result

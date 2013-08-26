(*********************************************************************************)
(*                OCaml-RDF                                                      *)
(*                                                                               *)
(*    Copyright (C) 2012-2013 Institut National de Recherche en Informatique     *)
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

(** Dump in graphviz format. *)

(** [apply_namespaces ns uri] return a pair [(prefix, suffix)] representing
  the given [uri] and using one of the namespaces [ns], of the form
  [(prefix, uri)].
  If a namespace [(pref, uri)] exists such that [uri] is a prefix of [uri2],
  then [apply_namespaces ns uri2 = (prefix, rel)], with [rel] being the
  suffix of [uri2] relative to [uri].
*)
val apply_namespaces : (string * string) list -> string -> string * string

(** [build_namespaces graph] returns the list namespaces of the [graph],
  as a list of pairs [(prefix, uri)] usable by {!apply_namespaces}.
  @param namespaces is used to specify additional namespaces. *)
val build_namespaces :
  ?namespaces:(Rdf_uri.uri * string) list ->
     Rdf_graph.graph -> (string * string) list

(** [dot_of_graph graph] returns the Graphviz code to represent the given [graph].
   @param namespaces is used so specified namespaces in the form [(uri, name)];
   in this case, the abbreviated form [ns:suffix] is used in labels of URI nodes,
   when possible. The namespaces of the graph are also used anyway.
   @param href can specify a function to call on each node, which can return
   an url (as a string), to add a [href] attribute to the node in the graphviz code.
   This is useful to provide clickable nodes in SVG output, for example.
*)
val dot_of_graph :
  ?namespaces:(Rdf_uri.uri * string) list ->
  ?href:(Rdf_term.term -> string option) -> Rdf_graph.graph -> string

(** Same as {!dot_of_graph} but return code to represent only the triples
  having the given URI as subject or object. *)
val dot_of_uri :
  ?namespaces:(Rdf_uri.uri * string) list -> ?href:(Rdf_term.term -> string option) ->
    Rdf_graph.graph -> Rdf_uri.uri -> string


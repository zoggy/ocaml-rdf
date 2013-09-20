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

(** Reading and writing Turtle. *)

type error = Parse_error of Rdf_loc.loc * string | Unknown_namespace of string
exception Error of error
val string_of_error : error -> string

(** Input graph from string. Default base is the graph name. *)
val from_string : Rdf_graph.graph -> ?base:Rdf_iri.iri -> string -> unit

(** Same as {!from_string} but read from the given file. *)
val from_file : Rdf_graph.graph -> ?base:Rdf_iri.iri -> string -> unit

val string_of_triple :
  sub:Rdf_term.term -> pred:Rdf_iri.iri -> obj:Rdf_term.term -> string

val to_ : ?namespaces: (Rdf_iri.iri * string) list ->
  (string -> unit) -> Rdf_graph.graph -> unit
val to_string : ?namespaces: (Rdf_iri.iri * string) list ->
  Rdf_graph.graph -> string
val to_file : ?namespaces: (Rdf_iri.iri * string) list ->
  Rdf_graph.graph -> string -> unit

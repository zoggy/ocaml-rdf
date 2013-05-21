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

(** Reading and writing RDF/XML. *)

exception Invalid_rdf of string

module SMap : Map.S with type key = string

(** The type of XML tree walked through to fill the graph. *)
type tree = E of Xmlm.tag * tree list | D of string

(** Type of current state when walking through the xml tree. *)
type state =
  { subject : Rdf_node.node option ;
    predicate : Rdf_uri.uri option ;
    xml_base : Rdf_uri.uri ;
    xml_lang : string option ;
    datatype : Rdf_uri.uri option ;
    namespaces : string Rdf_uri.Urimap.t ;
  }

(** Global state of the analysis. *)
type global_state =
  {
    blanks : Rdf_node.blank_id SMap.t ;
    gnamespaces : string Rdf_uri.Urimap.t ;
  }

val get_blank_node : Rdf_graph.graph -> global_state -> SMap.key -> Rdf_node.node * global_state

val input_node: Rdf_graph.graph -> state -> global_state -> tree -> global_state

(** Fill a graph from a current state, a pair (global state, li counter),
  and a property node. *)
val input_prop : Rdf_graph.graph -> state -> (global_state * int) -> tree -> (global_state * int)

val from_string : Rdf_graph.graph -> base: Rdf_uri.uri -> string -> unit
val from_file : Rdf_graph.graph -> base: Rdf_uri.uri -> string -> unit

val to_string :
  ?namespaces: (Rdf_uri.uri * string) list -> Rdf_graph.graph -> string

val to_file :
  ?namespaces: (Rdf_uri.uri * string) list ->
    Rdf_graph.graph -> string -> unit
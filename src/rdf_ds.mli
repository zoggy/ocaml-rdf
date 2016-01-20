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

(** Datasets.

A dataset is composed of:
- a default graph,
- a set of named graphs,
- a function to get a graph by its IRI.
*)

(** This exception indicates that a graph could not be retrieved.
  It can be raised by functions retrieving graphs, in a {!dataset}
  structure. The URI of the graph and an error message must be provided.
*)
exception Could_not_retrieve_graph of Iri.iri * string

(** This function raises the {!Could_not_retrieve_graph} exception. *)
val could_not_retrieve_graph : Iri.iri -> string -> 'a

(** A dataset. *)
type dataset = {
  default : Rdf_graph.graph; (** The default graph. *)
  named : Iri.Set.t; (** The set of named graphs. *)
  get_named : Iri.iri -> Rdf_graph.graph;
    (** The function to get a graph by its name (URI).
       The function must raise {!Could_not_retrieve_graph} in case of error. *)
}

(** [simple_dataset graph] returns a dataset with [graph] as default graph.
  @param named can be used to specify named graphs. The [get_named] function
  is created from this closed list of named graphs and raise {!Could_not_retrieve_graph}
  in case a required graph is not part of the list. *)
val simple_dataset :
  ?named:(Iri.iri * Rdf_graph.graph) list -> Rdf_graph.graph -> dataset

(** [dataset graph] returns a dataset with [graph] as default graph.
  @param named is used to specify the sef of named graphs, but it
  does not create a get_named function.
  @param get_named is the function to retrieve graph by name (URI). If it
  is not provided, the default function always raises {!Could_not_retrieve_graph}.
*)
val dataset :
  ?get_named:(Iri.iri -> Rdf_graph.graph) ->
    ?named:Iri.Set.t-> Rdf_graph.graph -> dataset

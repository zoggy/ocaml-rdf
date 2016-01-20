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

(** Elements of [http://www.w3.org/2000/01/rdf-schema#] *)

(** [http://www.w3.org/2000/01/rdf-schema#] *)
val rdfs : Iri.t
val rdfs_ : string -> Iri.t

(** The class of classes. *)
val rdfs_Class : Iri.t

(** A description of the subject resource. *)
val rdfs_comment : Iri.t

(** The class of RDF containers. *)
val rdfs_Container : Iri.t

(** The class of container membership properties, rdf:_1, rdf:_2, ..., all of which are sub-properties of 'member'. *)
val rdfs_ContainerMembershipProperty : Iri.t

(** The class of RDF datatypes. *)
val rdfs_Datatype : Iri.t

(** A domain of the subject property. *)
val rdfs_domain : Iri.t

(** The defininition of the subject resource. *)
val rdfs_isDefinedBy : Iri.t

(** A human-readable name for the subject. *)
val rdfs_label : Iri.t

(** The class of literal values, eg. textual strings and integers. *)
val rdfs_Literal : Iri.t

(** A member of the subject resource. *)
val rdfs_member : Iri.t

(** A range of the subject property. *)
val rdfs_range : Iri.t

(** The class resource, everything. *)
val rdfs_Resource : Iri.t

(** Further information about the subject resource. *)
val rdfs_seeAlso : Iri.t

(** The subject is a subclass of a class. *)
val rdfs_subClassOf : Iri.t

(** The subject is a subproperty of a property. *)
val rdfs_subPropertyOf : Iri.t

(** {2 Building vocabulary descriptions} *)

(** Add usual [rdf] and [rdfs] namespaces in the given graph. *)
val add_namespaces : Rdf_graph.graph -> unit

val property : Rdf_graph.graph ->
  label: string ->
    ?label_lang: (string * string) list ->
    ?comment: string ->
    ?comment_lang: (string * string) list ->
    ?domains: Iri.t list ->
    ?ranges: Iri.t list ->
    ?subof: Iri.t ->
    ?more: (Iri.t * Rdf_term.term) list ->
    Iri.t -> unit

val class_ : Rdf_graph.graph ->
  label: string ->
    ?label_lang: (string * string) list ->
    ?comment: string ->
    ?comment_lang: (string * string) list ->
    ?subof: Iri.t ->
    ?more: (Iri.t * Rdf_term.term) list ->
    Iri.t -> unit


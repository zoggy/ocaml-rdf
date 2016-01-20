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

(** Rdf constants.

  The others as defined in
  {{:http://www.w3.org/TR/rdf-syntax-grammar/}RDF grammar}. *)

open Iri

(** {3 Common namespaces} *)

val rdf : iri
val rdf_ : string -> iri

val xsd : iri
val xsd_ : string -> iri

val dc : iri
val dc_ : string -> iri

(** {3 Syntax names} *)

val rdf_about : iri
val rdf_datatype : iri
val rdf_Description : iri
val rdf_ID : iri
val rdf_li : iri
val rdf_nodeID : iri
val rdf_RDF : iri
val rdf_parseType : iri
val rdf_resource : iri

(** {3 Class names} *)

val rdf_Alt : iri
val rdf_Bag : iri
val rdf_List : iri
val rdf_Property : iri
val rdf_Seq : iri
val rdf_Statement : iri
val rdf_XMLLiteral : iri

(** {3 Property names} *)

val rdf_subject : iri
val rdf_predicate : iri
val rdf_object : iri
val rdf_type : iri
val rdf_value : iri
val rdf_first : iri
val rdf_rest : iri
val rdf_n : int -> iri

(** {3 Resource names} *)

val rdf_nil : iri

(** {3 XML Schema datatypes} *)

val xsd_integer : iri
val xsd_double : iri
val xsd_decimal : iri
val xsd_boolean : iri
val xsd_string : iri
val xsd_datetime : iri
val rdf_langstring : iri


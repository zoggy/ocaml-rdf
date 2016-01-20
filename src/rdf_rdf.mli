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

val rdf : t
val rdf_ : string -> t

val xsd : t
val xsd_ : string -> t

val dc : t
val dc_ : string -> t

(** {3 Syntax names} *)

val rdf_about : t
val rdf_datatype : t
val rdf_Description : t
val rdf_ID : t
val rdf_li : t
val rdf_nodeID : t
val rdf_RDF : t
val rdf_parseType : t
val rdf_resource : t

(** {3 Class names} *)

val rdf_Alt : t
val rdf_Bag : t
val rdf_List : t
val rdf_Property : t
val rdf_Seq : t
val rdf_Statement : t
val rdf_XMLLiteral : t

(** {3 Property names} *)

val rdf_subject : t
val rdf_predicate : t
val rdf_object : t
val rdf_type : t
val rdf_value : t
val rdf_first : t
val rdf_rest : t
val rdf_n : int -> t

(** {3 Resource names} *)

val rdf_nil : t

(** {3 XML Schema datatypes} *)

val xsd_integer : t
val xsd_double : t
val xsd_decimal : t
val xsd_boolean : t
val xsd_string : t
val xsd_datetime : t
val rdf_langstring : t


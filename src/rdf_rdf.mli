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

(** Rdf constants.

  The others as defined in
  {{:http://www.w3.org/TR/rdf-syntax-grammar/}RDF grammar}. *)

val rdf_ : string -> Rdf_uri.uri
val xsd_ : string -> Rdf_uri.uri

(** {3 Syntax names} *)

val rdf_about : Rdf_uri.uri
val rdf_datatype : Rdf_uri.uri
val rdf_Description : Rdf_uri.uri
val rdf_ID : Rdf_uri.uri
val rdf_li : Rdf_uri.uri
val rdf_nodeID : Rdf_uri.uri
val rdf_RDF : Rdf_uri.uri
val rdf_parseType : Rdf_uri.uri
val rdf_resource : Rdf_uri.uri

(** {3 Class names} *)

val rdf_Alt : Rdf_uri.uri
val rdf_Bag : Rdf_uri.uri
val rdf_List : Rdf_uri.uri
val rdf_Property : Rdf_uri.uri
val rdf_Seq : Rdf_uri.uri
val rdf_Statement : Rdf_uri.uri
val rdf_XMLLiteral : Rdf_uri.uri

(** {3 Property names} *)

val rdf_subject : Rdf_uri.uri
val rdf_predicate : Rdf_uri.uri
val rdf_object : Rdf_uri.uri
val rdf_type : Rdf_uri.uri
val rdf_value : Rdf_uri.uri
val rdf_first : Rdf_uri.uri
val rdf_rest : Rdf_uri.uri
val rdf_n : int -> Rdf_uri.uri

(** {3 Resource names} *)

val rdf_nil : Rdf_uri.uri

(** {3 XML Schema datatypes} *)

val xsd_integer : Rdf_uri.uri
val xsd_double : Rdf_uri.uri
val xsd_decimal : Rdf_uri.uri
val xsd_boolean : Rdf_uri.uri
val xsd_string : Rdf_uri.uri
val xsd_datetime : Rdf_uri.uri
val rdf_langstring : Rdf_uri.uri

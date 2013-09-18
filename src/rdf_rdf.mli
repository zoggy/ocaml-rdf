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

open Rdf_uri ;;

(** {3 Common namespaces} *)

val rdf : uri
val rdf_ : string -> uri

val xsd : uri
val xsd_ : string -> uri

val dc : uri
val dc_ : string -> uri

(** {3 Syntax names} *)

val rdf_about : uri
val rdf_datatype : uri
val rdf_Description : uri
val rdf_ID : uri
val rdf_li : uri
val rdf_nodeID : uri
val rdf_RDF : uri
val rdf_parseType : uri
val rdf_resource : uri

(** {3 Class names} *)

val rdf_Alt : uri
val rdf_Bag : uri
val rdf_List : uri
val rdf_Property : uri
val rdf_Seq : uri
val rdf_Statement : uri
val rdf_XMLLiteral : uri

(** {3 Property names} *)

val rdf_subject : uri
val rdf_predicate : uri
val rdf_object : uri
val rdf_type : uri
val rdf_value : uri
val rdf_first : uri
val rdf_rest : uri
val rdf_n : int -> uri

(** {3 Resource names} *)

val rdf_nil : uri

(** {3 XML Schema datatypes} *)

val xsd_integer : uri
val xsd_double : uri
val xsd_decimal : uri
val xsd_boolean : uri
val xsd_string : uri
val xsd_datetime : uri
val rdf_langstring : uri


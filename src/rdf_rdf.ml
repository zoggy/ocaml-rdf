(*********************************************************************************)
(*                OCaml-RDF                                                      *)
(*                                                                               *)
(*    Copyright (C) 2012 Institut National de Recherche en Informatique          *)
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

  One to keep namespaces in a graph.
  The others as defined in
  {{:http://www.w3.org/TR/rdf-syntax-grammar/}RDF grammar}. *)

(** This is the uri used as predicate to store namespaces in a graph.
  Having the namespaces in useful when generating rdf/xml. *)
let ordf_ns = Rdf_uri.uri "http://ocaml-rdf.forge.ocamlcore.org/ocamlrdf.rdf#namespace"

let rdf_ s = Rdf_uri.uri ("http://www.w3.org/1999/02/22-rdf-syntax-ns#"^s);;
let xsd_ s = Rdf_uri.uri ("http://www.w3.org/2001/XMLSchema#"^s);;

(** {3 Syntax names} *)

let rdf_about = rdf_"about";;
let rdf_datatype = rdf_"datatype";;
let rdf_Description = rdf_"Description";;
let rdf_ID = rdf_"ID";;
let rdf_li = rdf_"li";;
let rdf_nodeID = rdf_"nodeID";;
let rdf_RDF = rdf_"RDF";;
let rdf_parseType = rdf_"parseType";;
let rdf_resource = rdf_"resource";;

(** {3 Class names} *)

let rdf_Alt = rdf_"Alt";;
let rdf_Bag = rdf_"Bag";;
let rdf_List = rdf_"List";;
let rdf_Property = rdf_"Property";;
let rdf_Seq = rdf_"Seq";;
let rdf_Statement = rdf_"Statement";;
let rdf_XMLLiteral = rdf_"XMLLiteral";;

(** {3 Property names} *)

let rdf_subject = rdf_"subject";;
let rdf_predicate = rdf_"predicate";;
let rdf_object = rdf_"object";;
let rdf_type = rdf_"type";;
let rdf_value = rdf_"value";;
let rdf_first = rdf_"first";;
let rdf_rest = rdf_"rest";;
let rdf_n n = rdf_("_"^(string_of_int n));;

(** {3 Resource names} *)

let rdf_nil = rdf_"nil"

(** {3 XML Schema datatypes} *)

let xsd_integer = xsd_"integer";;
let xsd_double = xsd_"double";;
let xsd_decimal = xsd_"decimal";;
let xsd_boolean = xsd_"boolean";;

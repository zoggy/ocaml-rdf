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

(** *)

let rdf = Rdf_iri.iri "http://www.w3.org/1999/02/22-rdf-syntax-ns#";;
let rdf_ = Rdf_iri.append rdf ;;

let xsd = Rdf_iri.iri "http://www.w3.org/2001/XMLSchema#";;
let xsd_ = Rdf_iri.append xsd ;;

let rdf_about = rdf_"about";;
let rdf_datatype = rdf_"datatype";;
let rdf_Description = rdf_"Description";;
let rdf_ID = rdf_"ID";;
let rdf_li = rdf_"li";;
let rdf_nodeID = rdf_"nodeID";;
let rdf_RDF = rdf_"RDF";;
let rdf_parseType = rdf_"parseType";;
let rdf_resource = rdf_"resource";;


let rdf_Alt = rdf_"Alt";;
let rdf_Bag = rdf_"Bag";;
let rdf_List = rdf_"List";;
let rdf_Property = rdf_"Property";;
let rdf_Seq = rdf_"Seq";;
let rdf_Statement = rdf_"Statement";;
let rdf_XMLLiteral = rdf_"XMLLiteral";;


let rdf_subject = rdf_"subject";;
let rdf_predicate = rdf_"predicate";;
let rdf_object = rdf_"object";;
let rdf_type = rdf_"type";;
let rdf_value = rdf_"value";;
let rdf_first = rdf_"first";;
let rdf_rest = rdf_"rest";;
let rdf_n n = rdf_("_"^(string_of_int n));;


let rdf_nil = rdf_"nil"


let xsd_integer = xsd_"integer";;
let xsd_double = xsd_"double";;
let xsd_decimal = xsd_"decimal";;
let xsd_boolean = xsd_"boolean";;
let xsd_string = xsd_"string";;
let xsd_datetime = xsd_"dateTime";;
let rdf_langstring = rdf_"rdf:langString"

let dc = Rdf_iri.iri "http://purl.org/dc/elements/1.1/"
let dc_ = Rdf_iri.append dc ;;




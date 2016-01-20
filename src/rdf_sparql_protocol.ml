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

type in_dataset = {
  inds_default : Iri.of_string option;
  inds_named : Iri.of_string list ;
  }

let empty_dataset = { inds_default = None ; inds_named = [] }

type in_message = {
    in_query : string ;
    in_dataset : in_dataset ;
  }

type error =
  | Malformed_query of string
  | Query_request_refused of string
  | Error_other of string

type out_message =
| Ok
| Result of Rdf_sparql.query_result
| Error of error

let string_of_error = function
| Malformed_query s -> "Malformed query: "^s
| Query_request_refused s -> "Query request refused: "^s
| Error_other s -> s



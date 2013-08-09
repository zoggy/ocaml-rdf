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

(** Sparql queries. *)

type error =
| Parse_error of Rdf_loc.loc * string
| Value_error of Rdf_dt.error
| Eval_error of Rdf_sparql_eval.error
| Algebra_error of Rdf_sparql_algebra.error

exception Error of error

(** {2 Parsing and printing Sparql queries} *)

val string_of_error : error -> string

val parse_from_string : string -> Rdf_sparql_types.query
val parse_from_file : string -> Rdf_sparql_types.query

val string_of_query : Rdf_sparql_types.query -> string

(** {2 Executing queries} *)

type query_result =
  Bool of bool
| Solutions of Rdf_sparql_ms.mu list
| Graph of Rdf_graph.graph

val execute :
  base:Rdf_uri.uri -> Rdf_ds.dataset -> Rdf_sparql_types.query -> query_result


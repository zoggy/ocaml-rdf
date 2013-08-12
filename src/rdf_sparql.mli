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

(** {2 Error handling} *)

type error =
| Parse_error of Rdf_loc.loc * string
| Value_error of Rdf_dt.error
| Eval_error of Rdf_sparql_eval.error
| Algebra_error of Rdf_sparql_algebra.error
| Not_select
| Not_ask
| Not_construct
| Not_describe

exception Error of error

val string_of_error : error -> string

(** {2 Parsing and printing Sparql queries} *)

val parse_from_string : string -> Rdf_sparql_types.query
val parse_from_file : string -> Rdf_sparql_types.query

val string_of_query : Rdf_sparql_types.query -> string

(** {2 Executing queries} *)

type solution = Rdf_sparql_ms.mu

type query_result =
  Bool of bool
| Solutions of solution list
| Graph of Rdf_graph.graph

(** [execute ~base dataset q] executes the sparql query [q] on [dataset], using
  [base] as base uri. The form of the result depends on the kind of query:
  - Select queries return a [Solution solutions]
  - Ask queries return a [Bool bool]
  - Construct queries return [Graph g]. If a graph is provided, it is filled
    and the same graph is returned; else a new graph (in memory) is created,
    filled and returned. If the graph is created, it uri is the [base] uri provided.
  - Describe queries return ...[FIXME: to be completed].

  @raise {!Error} in case of error.
*)
val execute : ?graph: Rdf_graph.graph ->
  base:Rdf_uri.uri -> Rdf_ds.dataset -> Rdf_sparql_types.query -> query_result

(** Execute the given SELECT query.
  @raise [Not_select] is the query is not a SELECT.
*)
val select :
  base: Rdf_uri.uri -> Rdf_ds.dataset -> Rdf_sparql_types.query -> solution list

(** Execute the given CONSTRUCT query.
  @raise [Not_construct] is the query is not a CONSTRUCT.
*)
val construct : ?graph: Rdf_graph.graph ->
  base: Rdf_uri.uri -> Rdf_ds.dataset -> Rdf_sparql_types.query -> Rdf_graph.graph

(** Execute the given ASK query.
  @raise [Not_ask] is the query is not a ASK.
*)
val ask :
  base: Rdf_uri.uri -> Rdf_ds.dataset -> Rdf_sparql_types.query -> bool

(** Execute the given DESCRIBE query.
  @raise [Not_describe] is the query is not a DESCRIBE.
*)
val describe :
  base: Rdf_uri.uri -> Rdf_ds.dataset -> Rdf_sparql_types.query -> unit
  
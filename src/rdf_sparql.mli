(*********************************************************************************)
(*                OCaml-RDF                                                      *)
(*                                                                               *)
(*    Copyright (C) 2012-2016 Institut National de Recherche en Informatique     *)
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
| Not_get
| Not_update
| Not_implemented of string

exception Error of error

val string_of_error : error -> string

(** {2 Parsing and printing Sparql queries} *)

type query = Rdf_sparql_types.query
val query_from_string : string -> query
val query_from_file : string -> query

val string_of_query : query -> string

(** {2 Executing queries} *)

(** {3 Solutions} *)

type solution

(**/**)

val solution_of_mu : Rdf_sparql_ms.mu -> solution

(**/**)

(** [get_term solution varname] returns the {!Rdf_term.term} bound to
  [varname] in the solution.
  @raise Not_found if the variable is not bound. *)
val get_term : solution -> string -> Rdf_term.term

(** [is_bound solution varname] returns whether the given variable name
  is bound in the solution. *)
val is_bound : solution -> string -> bool

(** [solution_fold f sol acc] is
  [f var1 term1 (f var2 term2 (...) acc)], folding over the bindings
  of the solution.*)
val solution_fold : (string -> Rdf_term.term -> 'a-> 'a) -> solution -> 'a -> 'a

(** [solution_iter f solution] calls [f] on each pair [(varname, term)]
  of the [solution]. *)
val solution_iter : (string -> Rdf_term.term -> unit) -> solution -> unit

(** {4 Convenient functions to access solution bindings.}

All these functions can raise {!Rdf_dt.Error} exceptions in case
the term bounded to the variable name is not compatible with the
asked type.

The functions are just calls to Rdf_dt functions. For example,
{!get_int} retrieve the bounded term with {!get_term},
then calls {!Rdf_dt.of_term} to get a {!Rdf_dt.value}, than
calls {!Rdf_dt.int} to retrieve an [Int n] value, then return [n].
*)

val get_string : solution -> string -> string

(** See comment of {!Rdf_dt.iri}. *)
val get_iri : solution -> Iri.t -> string -> Iri.t
val get_int : solution -> string -> int
val get_float : solution -> string -> float
val get_bool : solution -> string -> bool
val get_datetime : solution -> string -> Rdf_term.datetime

(** Same as {!get_string} but the associated language tag is kep, if any. *)
val get_ltrl : solution -> string -> string * string option

(** {3 Querying} *)

type query_result =
  Bool of bool
| Solutions of solution list
| Graph of Rdf_graph.graph

(** [execute ~base dataset q] executes the sparql query [q] on [dataset], using
  [base] as base iri. The form of the result depends on the kind of query:
  - Select queries return a [Solution solutions]
  - Ask queries return a [Bool bool]
  - Construct queries return [Graph g].
  - Describe queries return a description graph.

  For [Construct] and [Describe] queries, if a graph is provided, it is filled
  and the same graph is returned; else a new graph (in memory) is created,
  filled and returned. If the graph is created, it iri is the [base] iri provided.

  {b Warning:} Describe queries are not implemented yet.

  @raise Error in case of error.
*)
val execute : ?graph: Rdf_graph.graph ->
  base:Iri.t -> Rdf_ds.dataset -> query -> query_result

val execute_update : graph: Rdf_graph.graph -> query -> query_result

(** {3 Convenient functions for querying} *)

(** Execute the given SELECT query.
  @raise Not_select is the query is not a SELECT.
*)
val select :
  base: Iri.t -> Rdf_ds.dataset -> query -> solution list

(** Execute the given CONSTRUCT query.
  @raise Not_construct is the query is not a CONSTRUCT.
*)
val construct : ?graph: Rdf_graph.graph ->
  base: Iri.t -> Rdf_ds.dataset -> query -> Rdf_graph.graph

(** Execute the given ASK query.
  @raise Not_ask is the query is not a ASK.
*)
val ask :
  base: Iri.t -> Rdf_ds.dataset -> query -> bool

(** Execute the given DESCRIBE query.
  @raise Not_describe is the query is not a DESCRIBE.
*)
val describe : ?graph: Rdf_graph.graph ->
  base: Iri.t -> Rdf_ds.dataset -> query -> Rdf_graph.graph

(** {2 Predefined functions}

These are the functions named by an IRI,
{{:http://www.w3.org/TR/sparql11-query/#expressions} see details here}. *)

(** A function takes a list of values and returns a value. *)
type iri_fun = Rdf_dt.value list -> Rdf_dt.value

val iri_funs : unit -> iri_fun Iri.Map.t
val add_iri_fun : Iri.t -> iri_fun -> unit

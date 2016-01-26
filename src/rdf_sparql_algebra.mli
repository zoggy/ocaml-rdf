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

module T = Rdf_sparql_types
type error =
    Variable_already_defined of Rdf_sparql_types.var
  | Unknown_prefix of Rdf_sparql_types.pname_ns
exception Error of error
val error : error -> 'a
val string_of_error : error -> string
type query = {
  query_proj : Rdf_sparql_types.select_clause option;
  query_where : Rdf_sparql_types.group_graph_pattern;
  query_modifier : Rdf_sparql_types.solution_modifier;
  query_values : Rdf_sparql_types.values_clause;
}
type filter = Rdf_sparql_types.constraint_
exception Implicit_aggregate_found
type path =
    Var of Rdf_sparql_types.var
  | Iri of Rdf_sparql_types.iriloc
  | Inv of path
  | Alt of path * path
  | Seq of path * path
  | ZeroOrMore of path
  | OneOrMore of path
  | ZeroOrOne of path
  | NPS of Rdf_sparql_types.iriloc list
type triple =
    Rdf_sparql_types.var_or_term * path * Rdf_sparql_types.var_or_term
module VS = Rdf_sparql_types.VarSet
type algebra =
    BGP of triple list
  | Join of algebra * algebra
  | LeftJoin of algebra * algebra * filter list
  | Filter of algebra * filter list
  | Union of algebra * algebra
  | Graph of Rdf_sparql_types.var_or_iri * algebra
  | Extend of algebra * Rdf_sparql_types.var * Rdf_sparql_types.expression
  | Minus of algebra * algebra
  | ToMultiset of algebra
  | DataToMultiset of Rdf_sparql_types.datablock
  | Group of Rdf_sparql_types.group_condition list * algebra
  | Aggregation of Rdf_sparql_types.aggregate
  | AggregateJoin of algebra * algebra list
  | Project of algebra * VS.t
  | Distinct of algebra
  | Reduced of algebra
  | Slice of algebra * int option * int option
  | OrderBy of algebra * Rdf_sparql_types.order_condition list
val visible_vars : query -> VS.t
val collect_and_remove_filters :
  Rdf_sparql_types.graph_pattern_elt list ->
  Rdf_sparql_types.constraint_ list * Rdf_sparql_types.graph_pattern_elt list
val fresh_var : unit -> Rdf_sparql_types.var
val path_iri_first : path
val path_iri_rest : path
val iri_nil : T.iri
val iri_type : Rdf_sparql_types.iriloc
val path_iri_type : path
val translate_path : Rdf_sparql_types.path -> path
val translate_path_sequence : Rdf_sparql_types.path_sequence -> path
val translate_path_elt_or_inverse : T.path_elt_or_inverse -> path
val translate_path_elt : T.path_elt -> path
val translate_path_primary : Rdf_sparql_types.path_primary -> path
val partition_path_one_in_prop_set :
  Rdf_sparql_types.path_one_in_prop_set list ->
  Rdf_sparql_types.iriloc list * Rdf_sparql_types.iriloc list
val translate_property_path_pattern :
  (T.var_or_term * path * T.var_or_term) list ->
  T.var_or_term * path * T.var_or_term ->
  (T.var_or_term * path * T.var_or_term) list
val translate_property_path_patterns :
  triple list -> (T.var_or_term * path * T.var_or_term) list
val build_triples_path :
  T.var_or_term -> triple list -> T.prop_object_list -> triple list
val build_triples_prop_graph_node :
  T.var_or_term ->
  path -> triple list -> Rdf_sparql_types.object_ -> triple list
val build_triples_path_collection :
  triple list ->
  T.var_or_term -> Rdf_sparql_types.object_ list -> triple list
val translate_triples_same_subject_path :
  triple list ->
  T.triples_same_subject -> (T.var_or_term * path * T.var_or_term) list
val translate_ggp : T.group_graph_pattern -> algebra
val translate_subselect : Rdf_sparql_types.sub_select -> algebra
val translate_ggp_sub : Rdf_sparql_types.ggp_sub -> algebra
val translate_triples_block : T.triples_block -> algebra
val translate_union : T.group_graph_pattern list -> algebra
val translate_service : T.service_graph_pattern -> algebra
val translate_inline_data : T.datablock -> algebra
val has_implicit_grouping : query -> bool
val aggregation_step :
  query ->
  algebra ->
  algebra * (Rdf_sparql_types.var * Rdf_sparql_types.expression) list * query
val translate_query_level : query -> algebra
val p : Buffer.t -> string -> unit
val string_of_var : Rdf_sparql_types.var -> string
val string_of_var_or_term : Rdf_sparql_types.var_or_term -> string
val string_of_path : path -> string
val string_of_triple :
  Rdf_sparql_types.var_or_term * path * Rdf_sparql_types.var_or_term ->
  string
val print_triple :
  string ->
  Buffer.t ->
  Rdf_sparql_types.var_or_term * path * Rdf_sparql_types.var_or_term -> unit
val print_triples :
  string ->
  Buffer.t ->
  (Rdf_sparql_types.var_or_term * path * Rdf_sparql_types.var_or_term) list ->
  unit
val print_expr : Buffer.t -> Rdf_sparql_types.expression -> unit
val print_group_condition :
  Buffer.t -> Rdf_sparql_types.group_condition -> unit
val print_order_cond : Buffer.t -> Rdf_sparql_types.order_condition -> unit
val print_order_conds :
  Buffer.t -> Rdf_sparql_types.order_condition list -> unit
val print : string -> Buffer.t -> algebra -> unit
val string_of_algebra : algebra -> string

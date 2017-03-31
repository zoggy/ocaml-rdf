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

val map_opt : ('a -> 'b) -> 'a option -> 'b option
module SMap = Rdf_xml.SMap
type env = { base : Iri.t; prefixes : Iri.t SMap.t; }
type dataset = { from : Iri.t list; from_named : Iri.Set.t; }
val create_env : Iri.t -> env
val iriref_a : Rdf_sparql_types.iri
val expand_relative_iri : env -> Iri.t -> Iri.t
val expand_iri : env -> Rdf_sparql_types.iri -> Rdf_sparql_types.iri
val expand_query_prolog_decl :
  env * Rdf_sparql_types.query_prolog_decl list ->
  Rdf_sparql_types.query_prolog_decl ->
  env * Rdf_sparql_types.query_prolog_decl list
val expand_query_prolog :
  env ->
  Rdf_sparql_types.query_prolog_decl list ->
  env * Rdf_sparql_types.query_prolog_decl list
val expand_rdf_literal :
  env -> Rdf_sparql_types.rdf_literal -> Rdf_sparql_types.rdf_literal
val expand_data_block_value :
  env ->
  Rdf_sparql_types.data_block_value -> Rdf_sparql_types.data_block_value
val expand_data_full_block_value :
  env ->
  Rdf_sparql_types.data_full_block_value ->
  Rdf_sparql_types.data_full_block_value
val expand_inline_data_one_var :
  env ->
  Rdf_sparql_types.inline_data_one_var ->
  Rdf_sparql_types.inline_data_one_var
val expand_inline_data_full :
  env ->
  Rdf_sparql_types.inline_data_full -> Rdf_sparql_types.inline_data_full
val expand_datablock :
  env -> Rdf_sparql_types.datablock -> Rdf_sparql_types.datablock
val expand_values_clause :
  env ->
  Rdf_sparql_types.datablock option -> Rdf_sparql_types.datablock option
val expand_var_or_iri :
  env -> Rdf_sparql_types.var_or_iri -> Rdf_sparql_types.var_or_iri
val expand_select_var :
  env -> Rdf_sparql_types.select_var -> Rdf_sparql_types.select_var
val expand_select_vars :
  env -> Rdf_sparql_types.select_vars -> Rdf_sparql_types.select_vars
val expand_select_clause :
  env -> Rdf_sparql_types.select_clause -> Rdf_sparql_types.select_clause
val expand_source_selector :
  env -> Rdf_sparql_types.source_selector -> Rdf_sparql_types.source_selector
val expand_dataset_clause :
  env -> Rdf_sparql_types.dataset_clause -> Rdf_sparql_types.dataset_clause
val expand_arg_list :
  env -> Rdf_sparql_types.arg_list -> Rdf_sparql_types.arg_list
val expand_function_call :
  env -> Rdf_sparql_types.function_call -> Rdf_sparql_types.function_call
val expand_expr : env -> Rdf_sparql_types.expr -> Rdf_sparql_types.expr
val expand_expression :
  env -> Rdf_sparql_types.expression -> Rdf_sparql_types.expression
val expand_aggregate :
  env -> Rdf_sparql_types.aggregate -> Rdf_sparql_types.aggregate
val expand_built_in_call :
  env -> Rdf_sparql_types.built_in_call -> Rdf_sparql_types.built_in_call
val expand_group_var :
  env -> Rdf_sparql_types.group_var -> Rdf_sparql_types.group_var
val expand_group_condition :
  env -> Rdf_sparql_types.group_condition -> Rdf_sparql_types.group_condition
val expand_constraint :
  env ->
  Rdf_sparql_types.having_condition -> Rdf_sparql_types.having_condition
val expand_having_condition :
  env ->
  Rdf_sparql_types.having_condition -> Rdf_sparql_types.having_condition
val expand_order_condition :
  env -> Rdf_sparql_types.order_condition -> Rdf_sparql_types.order_condition
val expand_limit_offset_clause :
  env ->
  Rdf_sparql_types.limit_offset_clause ->
  Rdf_sparql_types.limit_offset_clause
val expand_solution_modifier :
  env ->
  Rdf_sparql_types.solution_modifier -> Rdf_sparql_types.solution_modifier
val expand_bind : env -> Rdf_sparql_types.bind -> Rdf_sparql_types.bind
val expand_service_graph_pattern :
  env ->
  Rdf_sparql_types.service_graph_pattern ->
  Rdf_sparql_types.service_graph_pattern
val expand_graph_graph_pattern :
  env ->
  Rdf_sparql_types.graph_graph_pattern ->
  Rdf_sparql_types.graph_graph_pattern
val expand_graph_pattern_elt :
  env ->
  Rdf_sparql_types.graph_pattern_elt -> Rdf_sparql_types.graph_pattern_elt
val expand_graph_term :
  env -> Rdf_sparql_types.graph_term -> Rdf_sparql_types.graph_term
val expand_var_or_term :
  env -> Rdf_sparql_types.var_or_term -> Rdf_sparql_types.var_or_term
val expand_path_one_in_prop_set :
  env ->
  Rdf_sparql_types.path_one_in_prop_set ->
  Rdf_sparql_types.path_one_in_prop_set
val expand_path_primary :
  env -> Rdf_sparql_types.path_primary -> Rdf_sparql_types.path_primary
val expand_path_elt :
  env -> Rdf_sparql_types.path_elt -> Rdf_sparql_types.path_elt
val expand_path_elt_or_inverse :
  env ->
  Rdf_sparql_types.path_elt_or_inverse ->
  Rdf_sparql_types.path_elt_or_inverse
val expand_path_sequence :
  env -> Rdf_sparql_types.path_sequence -> Rdf_sparql_types.path_sequence
val expand_path : env -> Rdf_sparql_types.path -> Rdf_sparql_types.path
val expand_verb : env -> Rdf_sparql_types.verb -> Rdf_sparql_types.verb
val expand_triples_node :
  env -> Rdf_sparql_types.triples_node -> Rdf_sparql_types.triples_node
val expand_graph_node :
  env -> Rdf_sparql_types.object_ -> Rdf_sparql_types.object_
val expand_object :
  env -> Rdf_sparql_types.object_ -> Rdf_sparql_types.object_
val expand_prop_object_list :
  env ->
  Rdf_sparql_types.prop_object_list -> Rdf_sparql_types.prop_object_list
val expand_triples_block :
  env -> Rdf_sparql_types.triples_block -> Rdf_sparql_types.triples_block
val expand_triples_same_subject :
  env ->
  Rdf_sparql_types.triples_same_subject ->
  Rdf_sparql_types.triples_same_subject
val expand_ggp_sub :
  env -> Rdf_sparql_types.ggp_sub -> Rdf_sparql_types.ggp_sub
val expand_group_graph_pattern :
  env ->
  Rdf_sparql_types.group_graph_pattern ->
  Rdf_sparql_types.group_graph_pattern
val expand_sub_select :
  env -> Rdf_sparql_types.sub_select -> Rdf_sparql_types.sub_select
val expand_select_query :
  env -> Rdf_sparql_types.select_query -> Rdf_sparql_types.select_query
val expand_triples_template :
  env ->
  Rdf_sparql_types.triples_same_subject list ->
  Rdf_sparql_types.triples_same_subject list
val expand_construct_template :
  env ->
  Rdf_sparql_types.triples_same_subject list ->
  Rdf_sparql_types.triples_same_subject list
val expand_construct_where :
  env -> Rdf_sparql_types.construct_where -> Rdf_sparql_types.construct_where
val expand_construct_query :
  env -> Rdf_sparql_types.construct_query -> Rdf_sparql_types.construct_query
val expand_describe_query :
  env -> Rdf_sparql_types.describe_query -> Rdf_sparql_types.describe_query
val expand_ask_query :
  env -> Rdf_sparql_types.ask_query -> Rdf_sparql_types.ask_query
val expand_query_kind :
  env -> Rdf_sparql_types.query_kind -> Rdf_sparql_types.query_kind
val build_dataset : env -> Rdf_sparql_types.query_kind -> dataset
val expand_query :
  Iri.t -> Rdf_sparql_types.query -> Iri.t * dataset * Rdf_sparql_types.query
val expand_update_query :
  Iri.t -> Rdf_sparql_types.query -> Iri.t * Rdf_sparql_types.query

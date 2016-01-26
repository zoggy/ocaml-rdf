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
val map_opt : ('a -> 'b) -> 'a option -> 'b option
type ('acc, 't) map_fun = 'acc mapper -> 'acc -> 't -> 't
and 'a mapper = {
  var : ('a, Rdf_sparql_types.var) map_fun;
  iriref : ('a, Rdf_sparql_types.iriref) map_fun;
  prefixed_name : ('a, Rdf_sparql_types.prefixed_name) map_fun;
  iriloc : ('a, Rdf_sparql_types.iriloc) map_fun;
  iri : ('a, Rdf_sparql_types.iri) map_fun;
  rdf_literal : ('a, Rdf_sparql_types.rdf_literal) map_fun;
  data_block_value : ('a, Rdf_sparql_types.data_block_value) map_fun;
  data_full_block_value :
    ('a, Rdf_sparql_types.data_full_block_value) map_fun;
  inline_data_one_var : ('a, Rdf_sparql_types.inline_data_one_var) map_fun;
  inline_data_full : ('a, Rdf_sparql_types.inline_data_full) map_fun;
  datablock : ('a, Rdf_sparql_types.datablock) map_fun;
  values_clause : ('a, Rdf_sparql_types.values_clause) map_fun;
  var_or_iri : ('a, Rdf_sparql_types.var_or_iri) map_fun;
  blank_node : ('a, Rdf_sparql_types.blank_node) map_fun;
  select_var : ('a, Rdf_sparql_types.select_var) map_fun;
  select_vars : ('a, Rdf_sparql_types.select_vars) map_fun;
  select_clause : ('a, Rdf_sparql_types.select_clause) map_fun;
  dataset_clause : ('a, Rdf_sparql_types.dataset_clause) map_fun;
  arg_list : ('a, Rdf_sparql_types.arg_list) map_fun;
  function_call : ('a, Rdf_sparql_types.function_call) map_fun;
  binary_op : ('a, Rdf_sparql_types.binary_op) map_fun;
  expr : ('a, Rdf_sparql_types.expr) map_fun;
  expression : ('a, Rdf_sparql_types.expression) map_fun;
  built_in_call : ('a, Rdf_sparql_types.built_in_call) map_fun;
  aggregate : ('a, Rdf_sparql_types.aggregate) map_fun;
  group_var : ('a, Rdf_sparql_types.group_var) map_fun;
  group_condition : ('a, Rdf_sparql_types.group_condition) map_fun;
  constraint_ : ('a, Rdf_sparql_types.constraint_) map_fun;
  order_condition : ('a, Rdf_sparql_types.order_condition) map_fun;
  limit_offset_clause : ('a, Rdf_sparql_types.limit_offset_clause) map_fun;
  solution_modifier : ('a, Rdf_sparql_types.solution_modifier) map_fun;
  bind : ('a, Rdf_sparql_types.bind) map_fun;
  service_graph_pattern :
    ('a, Rdf_sparql_types.service_graph_pattern) map_fun;
  graph_graph_pattern : ('a, Rdf_sparql_types.graph_graph_pattern) map_fun;
  graph_pattern_elt : ('a, Rdf_sparql_types.graph_pattern_elt) map_fun;
  graph_term : ('a, Rdf_sparql_types.graph_term) map_fun;
  var_or_term : ('a, Rdf_sparql_types.var_or_term) map_fun;
  path_one_in_prop_set : ('a, Rdf_sparql_types.path_one_in_prop_set) map_fun;
  path_primary : ('a, Rdf_sparql_types.path_primary) map_fun;
  path_elt : ('a, Rdf_sparql_types.path_elt) map_fun;
  path_elt_or_inverse : ('a, Rdf_sparql_types.path_elt_or_inverse) map_fun;
  path_sequence : ('a, Rdf_sparql_types.path_sequence) map_fun;
  path : ('a, Rdf_sparql_types.path) map_fun;
  verb : ('a, Rdf_sparql_types.verb) map_fun;
  triples_node : ('a, Rdf_sparql_types.triples_node) map_fun;
  graph_node : ('a, Rdf_sparql_types.graph_node) map_fun;
  prop_object_list : ('a, Rdf_sparql_types.prop_object_list) map_fun;
  triples_block : ('a, Rdf_sparql_types.triples_block) map_fun;
  triples_same_subject : ('a, Rdf_sparql_types.triples_same_subject) map_fun;
  ggp_sub : ('a, Rdf_sparql_types.ggp_sub) map_fun;
  group_graph_pattern : ('a, Rdf_sparql_types.group_graph_pattern) map_fun;
  sub_select : ('a, Rdf_sparql_types.sub_select) map_fun;
}
val var : 'a -> 'b -> 'c -> 'c
val iriref : 'a -> 'b -> 'c -> 'c
val prefixed_name : 'a -> 'b -> 'c -> 'c
val iriloc : 'a -> 'b -> 'c -> 'c
val iri : 'a mapper -> 'a -> Rdf_sparql_types.iri -> Rdf_sparql_types.iri
val rdf_literal : 'a -> 'b -> 'c -> 'c
val data_block_value :
  'a mapper ->
  'a ->
  Rdf_sparql_types.data_block_value -> Rdf_sparql_types.data_block_value
val data_full_block_value :
  'a mapper ->
  'a ->
  Rdf_sparql_types.data_full_block_value ->
  Rdf_sparql_types.data_full_block_value
val inline_data_one_var :
  'a mapper ->
  'a ->
  Rdf_sparql_types.inline_data_one_var ->
  Rdf_sparql_types.inline_data_one_var
val inline_data_full :
  'a mapper ->
  'a ->
  Rdf_sparql_types.inline_data_full -> Rdf_sparql_types.inline_data_full
val datablock :
  'a mapper -> 'a -> Rdf_sparql_types.datablock -> Rdf_sparql_types.datablock
val values_clause :
  'a mapper ->
  'a ->
  Rdf_sparql_types.datablock option -> Rdf_sparql_types.datablock option
val var_or_iri :
  'a mapper ->
  'a -> Rdf_sparql_types.var_or_iri -> Rdf_sparql_types.var_or_iri
val blank_node : 'a -> 'b -> 'c -> 'c
val select_var :
  'a mapper ->
  'a -> Rdf_sparql_types.select_var -> Rdf_sparql_types.select_var
val select_vars :
  'a mapper ->
  'a -> Rdf_sparql_types.select_vars -> Rdf_sparql_types.select_vars
val select_clause :
  'a mapper ->
  'a -> Rdf_sparql_types.select_clause -> Rdf_sparql_types.select_clause
val dataset_clause :
  'a mapper ->
  'a -> Rdf_sparql_types.dataset_clause -> Rdf_sparql_types.dataset_clause
val arg_list :
  'a mapper -> 'a -> Rdf_sparql_types.arg_list -> Rdf_sparql_types.arg_list
val function_call :
  'a mapper -> 'a -> T.function_call -> Rdf_sparql_types.function_call
val binary_op : 'a -> 'b -> 'c -> 'c
val expr : 'a mapper -> 'a -> Rdf_sparql_types.expr -> Rdf_sparql_types.expr
val expression : 'a mapper -> 'a -> T.expression -> T.expression
val built_in_call :
  'a mapper ->
  'a -> Rdf_sparql_types.built_in_call -> Rdf_sparql_types.built_in_call
val aggregate :
  'a mapper -> 'a -> Rdf_sparql_types.aggregate -> Rdf_sparql_types.aggregate
val group_var :
  'a mapper -> 'a -> Rdf_sparql_types.group_var -> Rdf_sparql_types.group_var
val group_condition :
  'a mapper ->
  'a -> Rdf_sparql_types.group_condition -> Rdf_sparql_types.group_condition
val constraint_ :
  'a mapper ->
  'a -> Rdf_sparql_types.constraint_ -> Rdf_sparql_types.constraint_
val order_condition :
  'a mapper ->
  'a -> Rdf_sparql_types.order_condition -> Rdf_sparql_types.order_condition
val limit_offset_clause : 'a -> 'b -> 'c -> 'c
val solution_modifier :
  'a mapper ->
  'a ->
  Rdf_sparql_types.solution_modifier -> Rdf_sparql_types.solution_modifier
val bind : 'a mapper -> 'a -> Rdf_sparql_types.bind -> Rdf_sparql_types.bind
val service_graph_pattern :
  'a mapper ->
  'a ->
  Rdf_sparql_types.service_graph_pattern ->
  Rdf_sparql_types.service_graph_pattern
val graph_graph_pattern :
  'a mapper ->
  'a ->
  Rdf_sparql_types.graph_graph_pattern ->
  Rdf_sparql_types.graph_graph_pattern
val graph_pattern_elt :
  'a mapper ->
  'a ->
  Rdf_sparql_types.graph_pattern_elt -> Rdf_sparql_types.graph_pattern_elt
val graph_term :
  'a mapper ->
  'a -> Rdf_sparql_types.graph_term -> Rdf_sparql_types.graph_term
val var_or_term :
  'a mapper ->
  'a -> Rdf_sparql_types.var_or_term -> Rdf_sparql_types.var_or_term
val path_one_in_prop_set :
  'a mapper ->
  'a ->
  Rdf_sparql_types.path_one_in_prop_set ->
  Rdf_sparql_types.path_one_in_prop_set
val path_primary :
  'a mapper ->
  'a -> Rdf_sparql_types.path_primary -> Rdf_sparql_types.path_primary
val path_elt :
  'a mapper -> 'a -> Rdf_sparql_types.path_elt -> Rdf_sparql_types.path_elt
val path_elt_or_inverse :
  'a mapper ->
  'a ->
  Rdf_sparql_types.path_elt_or_inverse ->
  Rdf_sparql_types.path_elt_or_inverse
val path_sequence :
  'a mapper ->
  'a ->
  Rdf_sparql_types.path_elt_or_inverse list ->
  Rdf_sparql_types.path_elt_or_inverse list
val path :
  'a mapper ->
  'a ->
  Rdf_sparql_types.path_sequence list -> Rdf_sparql_types.path_sequence list
val verb : 'a mapper -> 'a -> Rdf_sparql_types.verb -> Rdf_sparql_types.verb
val triples_node :
  'a mapper ->
  'a -> Rdf_sparql_types.triples_node -> Rdf_sparql_types.triples_node
val graph_node :
  'a mapper ->
  'a -> Rdf_sparql_types.graph_node -> Rdf_sparql_types.graph_node
val prop_object_list :
  'a mapper ->
  'a ->
  Rdf_sparql_types.prop_object_list -> Rdf_sparql_types.prop_object_list
val triples_block :
  'a mapper ->
  'a -> Rdf_sparql_types.triples_block -> Rdf_sparql_types.triples_block
val triples_same_subject :
  'a mapper ->
  'a ->
  Rdf_sparql_types.triples_same_subject ->
  Rdf_sparql_types.triples_same_subject
val ggp_sub :
  'a mapper -> 'a -> Rdf_sparql_types.ggp_sub -> Rdf_sparql_types.ggp_sub
val group_graph_pattern :
  'a mapper ->
  'a ->
  Rdf_sparql_types.group_graph_pattern ->
  Rdf_sparql_types.group_graph_pattern
val sub_select :
  'a mapper ->
  'a -> Rdf_sparql_types.sub_select -> Rdf_sparql_types.sub_select
val default : 'a mapper

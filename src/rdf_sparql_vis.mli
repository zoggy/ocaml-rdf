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
val map_opt : ('a -> 'b -> 'a) -> 'a -> 'b option -> 'a
type ('acc, 't) visitor_fun = 'acc visitor -> 'acc -> 't -> 'acc
and 'a visitor = {
  var : ('a, Rdf_sparql_types.var) visitor_fun;
  iriref : ('a, Rdf_sparql_types.iriref) visitor_fun;
  prefixed_name : ('a, Rdf_sparql_types.prefixed_name) visitor_fun;
  iriloc : ('a, Rdf_sparql_types.iriloc) visitor_fun;
  iri : ('a, Rdf_sparql_types.iri) visitor_fun;
  rdf_literal : ('a, Rdf_sparql_types.rdf_literal) visitor_fun;
  data_block_value : ('a, Rdf_sparql_types.data_block_value) visitor_fun;
  data_full_block_value :
    ('a, Rdf_sparql_types.data_full_block_value) visitor_fun;
  inline_data_one_var :
    ('a, Rdf_sparql_types.inline_data_one_var) visitor_fun;
  inline_data_full : ('a, Rdf_sparql_types.inline_data_full) visitor_fun;
  datablock : ('a, Rdf_sparql_types.datablock) visitor_fun;
  values_clause : ('a, Rdf_sparql_types.values_clause) visitor_fun;
  var_or_iri : ('a, Rdf_sparql_types.var_or_iri) visitor_fun;
  blank_node : ('a, Rdf_sparql_types.blank_node) visitor_fun;
  select_var : ('a, Rdf_sparql_types.select_var) visitor_fun;
  select_vars : ('a, Rdf_sparql_types.select_vars) visitor_fun;
  select_clause : ('a, Rdf_sparql_types.select_clause) visitor_fun;
  dataset_clause : ('a, Rdf_sparql_types.dataset_clause) visitor_fun;
  arg_list : ('a, Rdf_sparql_types.arg_list) visitor_fun;
  function_call : ('a, Rdf_sparql_types.function_call) visitor_fun;
  binary_op : ('a, Rdf_sparql_types.binary_op) visitor_fun;
  expr : ('a, Rdf_sparql_types.expr) visitor_fun;
  expression : ('a, Rdf_sparql_types.expression) visitor_fun;
  built_in_call : ('a, Rdf_sparql_types.built_in_call) visitor_fun;
  aggregate : ('a, Rdf_sparql_types.aggregate) visitor_fun;
  group_var : ('a, Rdf_sparql_types.group_var) visitor_fun;
  group_condition : ('a, Rdf_sparql_types.group_condition) visitor_fun;
  constraint_ : ('a, Rdf_sparql_types.constraint_) visitor_fun;
  order_condition : ('a, Rdf_sparql_types.order_condition) visitor_fun;
  limit_offset_clause :
    ('a, Rdf_sparql_types.limit_offset_clause) visitor_fun;
  solution_modifier : ('a, Rdf_sparql_types.solution_modifier) visitor_fun;
  bind : ('a, Rdf_sparql_types.bind) visitor_fun;
  service_graph_pattern :
    ('a, Rdf_sparql_types.service_graph_pattern) visitor_fun;
  graph_graph_pattern :
    ('a, Rdf_sparql_types.graph_graph_pattern) visitor_fun;
  graph_pattern_elt : ('a, Rdf_sparql_types.graph_pattern_elt) visitor_fun;
  graph_term : ('a, Rdf_sparql_types.graph_term) visitor_fun;
  var_or_term : ('a, Rdf_sparql_types.var_or_term) visitor_fun;
  path_one_in_prop_set :
    ('a, Rdf_sparql_types.path_one_in_prop_set) visitor_fun;
  path_primary : ('a, Rdf_sparql_types.path_primary) visitor_fun;
  path_elt : ('a, Rdf_sparql_types.path_elt) visitor_fun;
  path_elt_or_inverse :
    ('a, Rdf_sparql_types.path_elt_or_inverse) visitor_fun;
  path_sequence : ('a, Rdf_sparql_types.path_sequence) visitor_fun;
  path : ('a, Rdf_sparql_types.path) visitor_fun;
  verb : ('a, Rdf_sparql_types.verb) visitor_fun;
  triples_node : ('a, Rdf_sparql_types.triples_node) visitor_fun;
  graph_node : ('a, Rdf_sparql_types.graph_node) visitor_fun;
  prop_object_list : ('a, Rdf_sparql_types.prop_object_list) visitor_fun;
  triples_block : ('a, Rdf_sparql_types.triples_block) visitor_fun;
  triples_same_subject :
    ('a, Rdf_sparql_types.triples_same_subject) visitor_fun;
  ggp_sub : ('a, Rdf_sparql_types.ggp_sub) visitor_fun;
  group_graph_pattern :
    ('a, Rdf_sparql_types.group_graph_pattern) visitor_fun;
  sub_select : ('a, Rdf_sparql_types.sub_select) visitor_fun;
}
val var : 'a -> 'b -> 'c -> 'b
val iriref : 'a -> 'b -> 'c -> 'b
val prefixed_name : 'a -> 'b -> 'c -> 'b
val iriloc : 'a -> 'b -> 'c -> 'b
val iri : 'a visitor -> 'a -> Rdf_sparql_types.iri -> 'a
val rdf_literal : 'a -> 'b -> 'c -> 'b
val data_block_value :
  'a visitor -> 'a -> Rdf_sparql_types.data_block_value -> 'a
val data_full_block_value :
  'a visitor -> 'a -> Rdf_sparql_types.data_full_block_value -> 'a
val inline_data_one_var :
  'a visitor -> 'a -> Rdf_sparql_types.inline_data_one_var -> 'a
val inline_data_full :
  'a visitor -> 'a -> Rdf_sparql_types.inline_data_full -> 'a
val datablock : 'a visitor -> 'a -> Rdf_sparql_types.datablock -> 'a
val values_clause :
  'a visitor -> 'a -> Rdf_sparql_types.datablock option -> 'a
val var_or_iri : 'a visitor -> 'a -> Rdf_sparql_types.var_or_iri -> 'a
val blank_node : 'a -> 'b -> 'c -> 'b
val select_var : 'a visitor -> 'a -> Rdf_sparql_types.select_var -> 'a
val select_vars : 'a visitor -> 'a -> Rdf_sparql_types.select_vars -> 'a
val select_clause : 'a visitor -> 'a -> Rdf_sparql_types.select_clause -> 'a
val dataset_clause :
  'a visitor -> 'a -> Rdf_sparql_types.dataset_clause -> 'a
val arg_list : 'a visitor -> 'a -> Rdf_sparql_types.arg_list -> 'a
val function_call : 'a visitor -> 'a -> T.function_call -> 'a
val binary_op : 'a -> 'b -> 'c -> 'b
val expr : 'a visitor -> 'a -> Rdf_sparql_types.expr -> 'a
val expression : 'a visitor -> 'a -> T.expression -> 'a
val built_in_call : 'a visitor -> 'a -> Rdf_sparql_types.built_in_call -> 'a
val aggregate : 'a visitor -> 'a -> Rdf_sparql_types.aggregate -> 'a
val group_var : 'a visitor -> 'a -> Rdf_sparql_types.group_var -> 'a
val group_condition :
  'a visitor -> 'a -> Rdf_sparql_types.group_condition -> 'a
val constraint_ : 'a visitor -> 'a -> Rdf_sparql_types.constraint_ -> 'a
val order_condition :
  'a visitor -> 'a -> Rdf_sparql_types.order_condition -> 'a
val limit_offset_clause : 'a -> 'b -> 'c -> 'b
val solution_modifier :
  'a visitor -> 'a -> Rdf_sparql_types.solution_modifier -> 'a
val bind : 'a visitor -> 'a -> Rdf_sparql_types.bind -> 'a
val service_graph_pattern :
  'a visitor -> 'a -> Rdf_sparql_types.service_graph_pattern -> 'a
val graph_graph_pattern :
  'a visitor -> 'a -> Rdf_sparql_types.graph_graph_pattern -> 'a
val graph_pattern_elt :
  'a visitor -> 'a -> Rdf_sparql_types.graph_pattern_elt -> 'a
val graph_term : 'a visitor -> 'a -> Rdf_sparql_types.graph_term -> 'a
val var_or_term : 'a visitor -> 'a -> Rdf_sparql_types.var_or_term -> 'a
val path_one_in_prop_set :
  'a visitor -> 'a -> Rdf_sparql_types.path_one_in_prop_set -> 'a
val path_primary : 'a visitor -> 'a -> Rdf_sparql_types.path_primary -> 'a
val path_elt : 'a visitor -> 'a -> Rdf_sparql_types.path_elt -> 'a
val path_elt_or_inverse :
  'a visitor -> 'a -> Rdf_sparql_types.path_elt_or_inverse -> 'a
val path_sequence :
  'a visitor -> 'a -> Rdf_sparql_types.path_elt_or_inverse list -> 'a
val path : 'a visitor -> 'a -> Rdf_sparql_types.path_sequence list -> 'a
val verb : 'a visitor -> 'a -> Rdf_sparql_types.verb -> 'a
val triples_node : 'a visitor -> 'a -> Rdf_sparql_types.triples_node -> 'a
val graph_node : 'a visitor -> 'a -> Rdf_sparql_types.graph_node -> 'a
val prop_object_list :
  'a visitor -> 'a -> Rdf_sparql_types.prop_object_list -> 'a
val triples_block : 'a visitor -> 'a -> Rdf_sparql_types.triples_block -> 'a
val triples_same_subject :
  'a visitor -> 'a -> Rdf_sparql_types.triples_same_subject -> 'a
val ggp_sub : 'a visitor -> 'a -> Rdf_sparql_types.ggp_sub -> 'a
val group_graph_pattern :
  'a visitor -> 'a -> Rdf_sparql_types.group_graph_pattern -> 'a
val sub_select : 'a visitor -> 'a -> Rdf_sparql_types.sub_select -> 'a
val default : 'a visitor

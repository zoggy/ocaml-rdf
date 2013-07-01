(** *)

open Rdf_sparql_types

type ('acc, 't) visitor_fun = 'acc visitor -> 'acc -> 't -> 'acc

and 'a visitor =
  {
    var : ('a, var) visitor_fun ;
    iriref : ('a, iriref) visitor_fun ;
    prefixed_name : ('a, prefixed_name) visitor_fun ;
    iri : ('a, iri) visitor_fun ;
    rdf_literal : ('a, rdf_literal) visitor_fun ;
    data_block_value : ('a, data_block_value) visitor_fun ;
    data_full_block_value : ('a, data_full_block_value ) visitor_fun ;
    inline_data_one_var : ('a, inline_data_one_var) visitor_fun ;
    inline_data_full : ('a, inline_data_full) visitor_fun ;
    datablock : ('a, datablock) visitor_fun ;
    values_clause : ('a, values_clause) visitor_fun ;
    var_or_iri : ('a, var_or_iri) visitor_fun ;
    blank_node : ('a, blank_node) visitor_fun ;
    select_var : ('a, select_var) visitor_fun ;
    select_vars : ('a, select_vars) visitor_fun ;
    select_clause : ('a, select_clause) visitor_fun ;
    dataset_clause : ('a, dataset_clause) visitor_fun ;
    arg_list : ('a, arg_list) visitor_fun ;
    function_call : ('a, function_call) visitor_fun ;
    binary_op : ('a, binary_op) visitor_fun ;
    expr : ('a, expr) visitor_fun ;
    expression : ('a, expression) visitor_fun ;
    built_in_call : ('a, built_in_call) visitor_fun ;
    aggregate : ('a, aggregate) visitor_fun ;
    group_var : ('a, group_var) visitor_fun ;
    group_condition : ('a, group_condition) visitor_fun ;
    constraint_ : ('a, constraint_) visitor_fun ;
    order_condition : ('a, order_condition) visitor_fun ;
    limit_offset_clause : ('a, limit_offset_clause) visitor_fun ;
    solution_modifier : ('a, solution_modifier) visitor_fun ;
    bind : ('a, bind) visitor_fun ;
    service_graph_pattern : ('a, service_graph_pattern) visitor_fun ;
    graph_graph_pattern : ('a, graph_graph_pattern) visitor_fun ;
    graph_pattern_elt : ('a, graph_pattern_elt) visitor_fun ;
    graph_term : ('a, graph_term) visitor_fun ;
    var_or_term : ('a, var_or_term) visitor_fun ;
    path_one_in_prop_set : ('a, path_one_in_prop_set) visitor_fun ;
    path_primary : ('a, path_primary) visitor_fun ;
    path_elt : ('a, path_elt) visitor_fun ;
    path_elt_or_inverse : ('a, path_elt_or_inverse) visitor_fun ;
    path_sequence : ('a, path_sequence) visitor_fun ;
    path : ('a, path) visitor_fun ;
    verb : ('a, verb) visitor_fun ;
    triples_node : ('a, triples_node) visitor_fun ;
    graph_node : ('a, graph_node) visitor_fun ;
    prop_object_list : ('a, prop_object_list) visitor_fun ;
    triples_block : ('a, triples_block) visitor_fun ;
    triples_same_subject : ('a, triples_same_subject) visitor_fun ;
    ggp_sub : ('a, ggp_sub) visitor_fun ;
    group_graph_pattern : ('a, group_graph_pattern) visitor_fun ;
    sub_select : ('a, sub_select) visitor_fun ;
  }

  
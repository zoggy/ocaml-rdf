val map_opt : ('a -> 'b) -> 'a option -> 'b option
val do_opt : ('a -> unit) -> 'a option -> unit
val p : Buffer.t -> string -> unit
val pp : Buffer.t -> ('a, Buffer.t, unit) format -> 'a
val print_list :
  ?sep:string -> Buffer.t -> (Buffer.t -> 'a -> unit) -> 'a list -> unit
val print_iriref : Buffer.t -> Rdf_sparql_types.iriref -> unit
val print_iriloc : Buffer.t -> Rdf_sparql_types.iriloc -> unit
val print_var : Buffer.t -> Rdf_sparql_types.var -> unit
val print_bnode : Buffer.t -> Rdf_sparql_types.blank_node -> unit
val print_path_mod : Buffer.t -> Rdf_sparql_types.path_mod -> unit
val print_iri : Buffer.t -> Rdf_sparql_types.iri -> unit
val print_query_prolog_decl :
  Buffer.t -> Rdf_sparql_types.query_prolog_decl -> unit
val print_query_prolog :
  Buffer.t -> Rdf_sparql_types.query_prolog_decl list -> unit
val print_string_lit : Buffer.t -> string -> unit
val print_rdf_literal : Buffer.t -> Rdf_sparql_types.rdf_literal -> unit
val print_data_block_value :
  Buffer.t -> Rdf_sparql_types.data_block_value -> unit
val print_data_full_block_value :
  Buffer.t -> Rdf_sparql_types.data_full_block_value -> unit
val print_inline_data_one_var :
  Buffer.t -> Rdf_sparql_types.inline_data_one_var -> unit
val print_inline_data_full :
  Buffer.t -> Rdf_sparql_types.inline_data_full -> unit
val print_datablock : Buffer.t -> Rdf_sparql_types.datablock -> unit
val print_values_clause :
  Buffer.t -> Rdf_sparql_types.datablock option -> unit
val print_var_or_iri : Buffer.t -> Rdf_sparql_types.var_or_iri -> unit
val print_select_var : Buffer.t -> Rdf_sparql_types.select_var -> unit
val print_select_vars : Buffer.t -> Rdf_sparql_types.select_vars -> unit
val print_select_clause : Buffer.t -> Rdf_sparql_types.select_clause -> unit
val print_source_selector :
  Buffer.t -> Rdf_sparql_types.source_selector -> unit
val print_dataset_clause :
  Buffer.t -> Rdf_sparql_types.dataset_clause -> unit
val print_arg_list : Buffer.t -> Rdf_sparql_types.arg_list -> unit
val print_function_call : Buffer.t -> Rdf_sparql_types.function_call -> unit
val string_of_bin_op : Rdf_sparql_types.binary_op -> string
val print_expr : Buffer.t -> Rdf_sparql_types.expr -> unit
val print_expression : Buffer.t -> Rdf_sparql_types.expression -> unit
val print_aggregate : Buffer.t -> Rdf_sparql_types.aggregate -> unit
val print_built_in_call : Buffer.t -> Rdf_sparql_types.built_in_call -> unit
val print_group_condition :
  Buffer.t -> Rdf_sparql_types.group_condition -> unit
val print_constraint : Buffer.t -> Rdf_sparql_types.having_condition -> unit
val print_having_condition :
  Buffer.t -> Rdf_sparql_types.having_condition -> unit
val print_order_condition :
  Buffer.t -> Rdf_sparql_types.order_condition -> unit
val print_limit_offset_clause :
  Buffer.t -> Rdf_sparql_types.limit_offset_clause -> unit
val print_solution_modifier :
  Buffer.t -> Rdf_sparql_types.solution_modifier -> unit
val print_bind : Buffer.t -> Rdf_sparql_types.bind -> unit
val print_service_graph_pattern :
  Buffer.t -> Rdf_sparql_types.service_graph_pattern -> unit
val print_graph_graph_pattern :
  Buffer.t -> Rdf_sparql_types.graph_graph_pattern -> unit
val print_graph_pattern_elt :
  Buffer.t -> Rdf_sparql_types.graph_pattern_elt -> unit
val print_graph_term : Buffer.t -> Rdf_sparql_types.graph_term -> unit
val print_var_or_term : Buffer.t -> Rdf_sparql_types.var_or_term -> unit
val print_path_one_in_prop_set :
  Buffer.t -> Rdf_sparql_types.path_one_in_prop_set -> unit
val print_path_primary : Buffer.t -> Rdf_sparql_types.path_primary -> unit
val print_path_elt : Buffer.t -> Rdf_sparql_types.path_elt -> unit
val print_path_elt_or_inverse :
  Buffer.t -> Rdf_sparql_types.path_elt_or_inverse -> unit
val print_path_sequence : Buffer.t -> Rdf_sparql_types.path_sequence -> unit
val print_path : Buffer.t -> Rdf_sparql_types.path -> unit
val print_verb : Buffer.t -> Rdf_sparql_types.verb -> unit
val print_triples_node : Buffer.t -> Rdf_sparql_types.triples_node -> unit
val print_graph_node : Buffer.t -> Rdf_sparql_types.object_ -> unit
val print_object : Buffer.t -> Rdf_sparql_types.object_ -> unit
val print_prop_object_list :
  Buffer.t -> Rdf_sparql_types.prop_object_list -> unit
val print_triples_same_subject :
  Buffer.t -> Rdf_sparql_types.triples_same_subject -> unit
val print_triples_block : Buffer.t -> Rdf_sparql_types.triples_block -> unit
val print_ggp_sub : Buffer.t -> Rdf_sparql_types.ggp_sub -> unit
val print_group_graph_pattern :
  Buffer.t -> Rdf_sparql_types.group_graph_pattern -> unit
val print_sub_select : Buffer.t -> Rdf_sparql_types.sub_select -> unit
val print_select_query : Buffer.t -> Rdf_sparql_types.select_query -> unit
val print_triples_template :
  Buffer.t -> Rdf_sparql_types.triples_same_subject list -> unit
val print_construct_template :
  Buffer.t -> Rdf_sparql_types.triples_same_subject list -> unit
val print_construct_where :
  Buffer.t -> Rdf_sparql_types.construct_where -> unit
val print_construct_query :
  Buffer.t -> Rdf_sparql_types.construct_query -> unit
val print_describe_query :
  Buffer.t -> Rdf_sparql_types.describe_query -> unit
val print_ask_query : Buffer.t -> Rdf_sparql_types.ask_query -> unit
val print_query_kind : Buffer.t -> Rdf_sparql_types.query_kind -> unit
val print_query : Buffer.t -> Rdf_sparql_types.query -> unit

(** *)

type loc =
  { loc_start : Lexing.position ;
    loc_end : Lexing.position ;
  }

type pname_ns = {
    pname_ns_loc : loc ;
    pname_ns_name : string ;
  }
;;

type pname_local = {
    pname_local_loc : loc ;
    pname_local_name : string ;
  }

type var = {
  var_loc : loc ;
  var_name : string ;
  }

type iriref =
  { ir_loc : loc ;
    ir_iri : Rdf_uri.uri ;
  }

type prefixed_name =
  { pname_loc : loc ;
    pname_ns : pname_ns ;
    pname_local : pname_local option ;
  }

type iri =
  | Iriref of iriref
  | PrefixedName of prefixed_name
;;

type prefix_decl = pname_ns * iriref ;;

type query_prolog_decl =
  | BaseDecl of iriref
  | PrefixDecl of prefix_decl
;;

type query_prolog = query_prolog_decl list ;;

type rdf_literal =
  { rdf_lit_loc : loc ;
    rdf_lit : Rdf_node.literal ;
    rdf_lit_type : iri option ; (* must be resolved after parsing *)
  }
;;

type data_block_value =
  | DataBlockValueIri of iri
  | DataBlockValueRdf of rdf_literal
  | DataBlockValueNumeric of rdf_literal
  | DataBlockValueBoolean of rdf_literal
  | DataBlockValueUndef
;;

type data_full_block_value =
  | Nil
  | Value of data_block_value list
;;

type inline_data_one_var =
  { idov_loc : loc ;
    idov_var : var ;
    idov_data : data_block_value list ;
  }
;;

type inline_data_full =
  { idf_loc : loc ;
    idf_vars : var list ;
    idf_values : data_full_block_value list ;
  }
;;

type datablock =
  | InLineDataOneVar of inline_data_one_var
  | InLineDataFull of inline_data_full

type values_clause = datablock option;;

type path_mod = ModOptional | ModList | ModOneOrMore ;;

type var_or_iri =
  | VIVar of var
  | VIIri of iri
;;

type blank_node =
  { bnode_loc : loc ;
    bnode_label : string option ;
  }
;;
type select_clause_flag = Distinct | Reduced ;;

type select_var =
  { sel_var_loc : loc ;
    sel_var_expr : expression option ;
    sel_var : var ;
  }

and select_vars =
  | SelectAll
  | SelectVars of select_var list

and select_clause = {
  sel_flag : select_clause_flag option ;
  sel_vars : select_vars ;
  }

and source_selector = iri

and dataset_clause =
  | DefaultGraphClause of source_selector
  | NamedGraphClause of  source_selector

and arg_list =
  { argl_loc : loc ;
    argl_distinct : bool ;
    argl : expression list ;
  }
and function_call =
  { func_loc : loc ;
    func_iri : iri ;
    func_args : arg_list ;
  }

and relational_expression =
  | Numexp of numeric_expression
  | Equal of numeric_expression * numeric_expression
  | NotEqual of numeric_expression * numeric_expression
  | Lt of numeric_expression * numeric_expression
  | Gt of numeric_expression * numeric_expression
  | Lte of numeric_expression * numeric_expression
  | Gte of numeric_expression * numeric_expression
  | In of numeric_expression * expression list
  | NotIn of numeric_expression * expression list

and numeric_expression = add_expression
and add_expression = mult_expression * add_expression2 list
and add_expression2 =
  | ExpPlus of mult_expression * add_expression3 list
  | ExpMinus of mult_expression * add_expression3 list
  | ExpPosNumeric of rdf_literal * add_expression3 list
  | ExpNegNumeric of rdf_literal * add_expression3 list
and add_expression3 =
   | AddMult of unary_expression
   | AddDiv of unary_expression

and mult_expression =
  | Unary of unary_expression
  | Mult of unary_expression * mult_expression
  | Div of unary_expression * mult_expression

and unary_expression =
  | Primary of primary_expression
  | PrimNot of primary_expression
  | PrimPlus of primary_expression
  | PrimMinus of primary_expression

and primary_expression =
  | PrimExpr of expression
  | PrimBuiltInCall of built_in_call
  | PrimFun of function_call
  | PrimLit of rdf_literal
  | PrimNumeric of rdf_literal
  | PrimBoolean of rdf_literal
  | PrimVar of var

and expression =
  { expr_loc : loc ;
    expr_or_exprs : and_expression list ; (* exp1 or exp2 ... *)
  }
and and_expression = value_logical list
and value_logical = relational_expression

and built_in_call =
  | Bic_COUNT of bool * expression option  (** '*' or expression *)
  | Bic_SUM of bool * expression
  | Bic_MIN of bool * expression
  | Bic_MAX of bool * expression
  | Bic_AVG of bool * expression
  | Bic_SAMPLE of bool * expression
  | Bic_GROUP_CONCAT of bool * expression * string option (* distinct * expr * separator option *)
  | Bic_STR of expression
  | Bic_LANG of expression
  | Bic_LANGMATCHES of expression * expression
  | Bic_DATATYPE of expression
  | Bic_BOUND of var
  | Bic_IRI of expression
  | Bic_URI of expression
  | Bic_BNODE of expression option
  | Bic_RAND
  | Bic_ABS of expression
  | Bic_CEIL of expression
  | Bic_FLOOR of expression
  | Bic_ROUND of expression
  | Bic_CONCAT of expression list
  | Bic_SUBSTR of expression * expression * expression option
  | Bic_STRLEN of expression
  | Bic_REPLACE of expression * expression * expression * expression option
  | Bic_UCASE of expression
  | Bic_LCASE of expression
  | Bic_ENCODE_FOR_URI of expression
  | Bic_CONTAINS of expression * expression
  | Bic_STRSTARTS of expression * expression
  | Bic_STRENDS of expression * expression
  | Bic_STRBEFORE of expression * expression
  | Bic_STRAFTER of expression * expression
  | Bic_YEAR of expression
  | Bic_MONTH of expression
  | Bic_DAY of expression
  | Bic_HOURS of expression
  | Bic_MINUTES of expression
  | Bic_SECONDS of expression
  | Bic_TIMEZONE of expression
  | Bic_TZ of expression
  | Bic_NOW
  | Bic_UUID
  | Bic_STRUUID
  | Bic_MD5 of expression
  | Bic_SHA1 of expression
  | Bic_SHA256 of expression
  | Bic_SHA384 of expression
  | Bic_SHA512 of expression
  | Bic_COALESCE of expression list
  | Bic_IF of expression * expression * expression
  | Bic_STRLANG of expression * expression
  | Bic_STRDT of expression * expression
  | Bic_SAMETERM of expression * expression
  | Bic_ISIRI of expression
  | Bic_ISURI of expression
  | Bic_ISBLANK of expression
  | Bic_ISLITERAL of expression
  | Bic_ISNUMERIC of expression
  | Bic_REGEXP of expression * expression * expression option
  | Bic_EXISTS of group_graph_pattern
  | Bic_NOTEXISTS of group_graph_pattern

and group_var =
  { grpvar_loc : loc ;
    grpvar_expr : expression option ;
    grpvar : var option ;
  }

and group_condition =
  | GroupBuiltInCall of built_in_call
  | GroupFunctionCall of function_call
  | GroupVar of group_var

and constraint_ =
  | ConstrBuiltInCall of built_in_call
  | ConstrFunctionCall of function_call
  | ConstrExpr of expression

and having_condition = constraint_

and order_condition =
  | OrderAsc of expression
  | OrderDesc of expression
  | OrderConstr of constraint_
  | OrderVar of var

and limit_offset_clause =
  { limoff_loc : loc ;
    limoff_offset : int option ;
    limoff_limit : int option ;
  }

and solution_modifier =
  { solmod_loc : loc ;
    solmod_group : group_condition list;
    solmod_having : having_condition list ;
    solmod_order : order_condition list option ;
    solmod_limoff : limit_offset_clause option ;
  }

and bind =
  { bind_loc : loc ;
    bind_expr : expression ;
    bind_var : var ;
  }

and service_graph_pattern =
  { servgp_loc : loc ;
    servgp_silent : bool ;
    servgp_name : var_or_iri ;
    servgp_pat : group_graph_pattern ;
  }

and graph_graph_pattern =
  { graphgp_loc : loc ;
    graphgp_name : var_or_iri ;
    graphgp_pat : group_graph_pattern ;
  }

and graph_pattern_not_triples =
  | Union of group_graph_pattern list
  | Optional of group_graph_pattern
  | Minus of group_graph_pattern
  | GGP of graph_graph_pattern
  | Service of service_graph_pattern
  | Filter of constraint_
  | Bind of bind
  | InlineData of datablock

and graph_term =
  | GraphTermIri of iri
  | GraphTermLit of rdf_literal
  | GraphTermNumeric of rdf_literal
  | GraphTermBoolean of rdf_literal
  | GraphTermBlank of blank_node
  | GraphTermNil

and var_or_term =
  | Var of var
  | GraphTerm of graph_term

and path_one_in_prop_set =
  | PathOneInIri of iri
  | PathOneInA
  | PathOneInNotIri of iri
  | PathOneInNotA

and path_primary =
  | PathIri of iri
  | PathA
  | PathNegPropSet of path_one_in_prop_set list
  | Path of path

and path_elt = {
    pelt_loc : loc ;
    pelt_primary : path_primary ;
    pelt_mod : path_mod option ;
  }

and path_elt_or_inverse =
  | Elt of path_elt
  | Inv of path_elt

and path_alternative = path_elt_or_inverse list

and path = path_alternative list

and verb_path =
  | VerbPath of path
  | VerbSimple of var

and verb =
  | VerbVar of var
  | VerbIri of iri
  | VerbA

and triples_node =
  | TNodeCollection of graph_node list
  | TNodeBlank of verb prop_object_list list

and graph_node =
  | GraphNodeVT of var_or_term
  | GraphNodeTriples of triples_node

and triples_node_path =
  | TNodePathCollection of graph_node_path list
  | TNodePathBlank of property_list_path

and graph_node_path =
  | GraphNodePathVT of var_or_term
  | GraphNodePathTriples of triples_node_path

and object_ = graph_node
and object_path = graph_node_path

and 'a prop_object_list =
  { propol_loc : loc ;
    propol_verb : 'a ;
    propol_objects : object_ list ;
  }

and property_list_path =
  { proplp_loc : loc ;
    proplp_verb : verb_path ;
    proplp_objects : object_path list ;
    proplp_more : verb_path prop_object_list list ;
  }

and triples_var_or_term_props =
  { tvtp_loc : loc ;
    tvtp_subject : var_or_term ;
    tvtp_path : property_list_path ;
  }

and triples_node_path_props =
  { tnpp_loc : loc ;
    tnpp_path : triples_node_path ;
    tnpp_props : property_list_path option;
  }

and triples_same_subject_path =
  | TriplesVar of triples_var_or_term_props
  | TriplesNodePath of triples_node_path_props

and triples_block =
  { triples_loc : loc ;
    triples : triples_same_subject_path list ;
  }

and ggp_sub = {
  ggp_sub_loc : loc ;
  ggp_sub_triples : triples_block option ;
  ggp_sub_rest : (graph_pattern_not_triples * triples_block option) list ;
  }

and group_graph_pattern =
  | SubSelect of sub_select
  | GGPSub of ggp_sub

and sub_select =
  { subsel_loc : loc ;
    subsel_select : select_clause ;
    subsel_where : group_graph_pattern ;
    subsel_modifier : solution_modifier ;
    subsel_values : values_clause ;
  }
;;

type select_query = {
    select_select : select_clause ;
    select_dataset : dataset_clause list ;
    select_where : group_graph_pattern ;
    select_modifier : solution_modifier ;
  }

type ask_query = {
    ask_dataset : dataset_clause list ;
    ask_where : group_graph_pattern ;
    ask_modifier : solution_modifier ;
  }

type query_kind =
  | Select of select_query
  | Construct
  | Describe
  | Ask of ask_query
;;


type query =
  { q_prolog : query_prolog ;
    q_kind : query_kind ;
    q_values : values_clause ;
  }
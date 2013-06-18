(** Expanding IRIs and triples in query abstract syntax tree. *)

open Rdf_sparql_types

let map_opt f = function
  None -> None
| Some x -> Some (f x)
;;

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

let iriref_a =
  Iriref
    { ir_loc = Rdf_sparql_types.dummy_loc ;
      ir_iri = Rdf_rdf.rdf_type ;
    }
;;

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

and expand_select_vars =
  | SelectAll
  | SelectVars of select_var list

and expand_select_clause = {
  sel_flag : select_clause_flag option ;
  sel_vars : select_vars ;
  }

and expand_source_selector = iri

and expand_dataset_clause =
  | DefaultGraphClause of source_selector
  | NamedGraphClause of  source_selector

and expand_arg_list =
  { argl_loc : loc ;
    argl_distinct : bool ;
    argl : expression list ;
  }
and expand_function_call =
  { func_loc : loc ;
    func_iri : iri ;
    func_args : arg_list ;
  }

and expand_relational_expression =
  | Numexp of numeric_expression
  | Equal of numeric_expression * numeric_expression
  | NotEqual of numeric_expression * numeric_expression
  | Lt of numeric_expression * numeric_expression
  | Gt of numeric_expression * numeric_expression
  | Lte of numeric_expression * numeric_expression
  | Gte of numeric_expression * numeric_expression
  | In of numeric_expression * expression list
  | NotIn of numeric_expression * expression list

and expand_numeric_expression = add_expression
and expand_add_expression = mult_expression * add_expression2 list
and expand_add_expression2 =
  | ExpPlus of mult_expression * add_expression3 list
  | ExpMinus of mult_expression * add_expression3 list
  | ExpPosNumeric of rdf_literal * add_expression3 list
  | ExpNegNumeric of rdf_literal * add_expression3 list
and expand_add_expression3 =
   | AddMult of unary_expression
   | AddDiv of unary_expression

and expand_mult_expression =
  | Unary of unary_expression
  | Mult of unary_expression * mult_expression
  | Div of unary_expression * mult_expression

and expand_unary_expression =
  | Primary of primary_expression
  | PrimNot of primary_expression
  | PrimPlus of primary_expression
  | PrimMinus of primary_expression

and expand_primary_expression =
  | PrimExpr of expression
  | PrimBuiltInCall of built_in_call
  | PrimFun of function_call
  | PrimLit of rdf_literal
  | PrimNumeric of rdf_literal
  | PrimBoolean of rdf_literal
  | PrimVar of var

and expand_expression =
  { expr_loc : loc ;
    expr_or_exprs : and_expression list ; (* exp1 or exp2 ... *)
  }
and expand_and_expression = value_logical list
and expand_value_logical = relational_expression

and expand_built_in_call env = function
| Bic_COUNT (b, eopt) ->
    Bic_COUNT (b, map_opt expand_expression env eopt)
| Bic_SUM (b, e) ->
    Bic_SUM (b, expand_expression env e)
| Bic_MIN (b, e) ->
    Bic_MIN (b, expand_expression env e)
| Bic_MAX (b, e) ->
    Bic_MAX (b, expand_expression env e)
| Bic_AVG (b, e) ->
    Bic_AVG (b, expand_expression env e)
| Bic_SAMPLE (b, e) ->
    Bic_SAMPLE (b, expand_expression env e)
| Bic_GROUP_CONCAT (b, e, s_opt) ->
    Bic_GROUP_CONCAT (b, expand_expression env e, s_opt)
| Bic_STR e ->
    Bic_STR (expand_expression env e)
| Bic_LANG e ->
    Bic_LANG (expand_expression env e)
| Bic_LANGMATCHES (e1, e2) ->
    Bic_LANGMATCHES (expand_expression env e1, expand_expression env e2)
| Bic_DATATYPE e ->
    Bic_DATATYPE (expand_expression env e)
| Bic_BOUND v -> Bic_BOUND v
| Bic_IRI e ->
    Bic_IRI (expand_expression env e)
| Bic_URI e ->
    Bic_URI (expand_expression env e)
| Bic_BNODE opt ->
    Bic_BNODE (map_opt (expand_expression env) opt)
| Bic_RAND -> Bic_RAND
| Bic_ABS e ->
    Bic_ABS (expand_expression env e)
| Bic_CEIL e ->
    Bic_CEIL (expand_expression env e)
| Bic_FLOOR e ->
    Bic_FLOOR (expand_expression env e)
| Bic_ROUND e ->
    Bic_ROUND (expand_expression env e)
| Bic_CONCAT l ->
    Bic_CONCAT (List.map (expand_expression env) l)
| Bic_SUBSTR (e1, e2, eopt)
      Bic_SUBSTR
      (expand_expression env e1,
       expand_expression env e2,
       map_opt (expand_expression env) eopt
      )
| Bic_STRLEN e ->
    Bic_STRLEN (expand_expression env e)
| Bic_REPLACE (e1, e2, e3, eopt)
      Bic_REPLACE
      (expand_expression env e1,
       expand_expression env e2,
       expand_expression env e3,
       map_opt (expand_expression env) eopt
      )
| Bic_UCASE e ->
    Bic_UCASE (expand_expression env e)
| Bic_LCASE e ->
    Bic_LCASE (expand_expression env e)
| Bic_ENCODE_FOR_URI e ->
    Bic_ENCODE_FOR_URI (expand_expression env e)
| Bic_CONTAINS (e1, e2) ->
    Bic_CONTAINS (expand_expression env e1, expand_expression env e2)
| Bic_STRSTARTS (e1, e2) ->
    Bic_STRSTARTS (expand_expression env e1, expand_expression env e2)
| Bic_STRENDS (e1, e2) ->
    Bic_STRENDS (expand_expression env e1, expand_expression env e2)
| Bic_STRBEFORE (e1, e2) ->
    Bic_STRBEFORE (expand_expression env e1, expand_expression env e2)
| Bic_STRAFTER (e1, e2) ->
    Bic_STRAFTER (expand_expression env e1, expand_expression env e2)
| Bic_YEAR e ->
    Bic_YEAR (expand_expression env e)
| Bic_MONTH e ->
    Bic_MONTH (expand_expression env e)
| Bic_DAY e ->
    Bic_DAY (expand_expression env e)
| Bic_HOURS e ->
    Bic_HOURS (expand_expression env e)
| Bic_MINUTES e ->
    Bic_MINUTES (expand_expression env e)
| Bic_SECONDS e ->
    Bic_SECONDS (expand_expression env e)
| Bic_TIMEZONE e ->
    Bic_TIMEZONE (expand_expression env e)
| Bic_TZ e ->
    Bic_TZ (expand_expression env e)
| Bic_NOW -> Bic_NOW
| Bic_UUID -> Bic8UUID
| Bic_STRUUID -> Bic_STRUUID
| Bic_MD5 e ->
    Bic_MD5 (expand_expression env e)
| Bic_SHA1 e ->
    Bic_SHA1 (expand_expression env e)
| Bic_SHA256 e ->
    Bic_SHA256 (expand_expression env e)
| Bic_SHA384 e ->
    Bic_SHA384 (expand_expression env e)
| Bic_SHA512 e ->
    Bic_SHA512 (expand_expression env e)
| Bic_COALESCE l ->
    Bic_COALESCE (List.map (expand_expression env) l)
| Bic_IF (e1, e2, e3) ->
    Bic_IF
      (expand_expression env e1,
       expand_expression env e2,
       expand_expression env e3
      )
| Bic_STRLANG (e1, e2) ->
    Bic_STRLANG (expand_expression env e1, expand_expression env e2)
| Bic_STRDT (e1, e2) ->
    Bic_STRDT (expand_expression env e1, expand_expression env e2)
| Bic_SAMETERM (e1, e2) ->
    Bic_SAMETERM(expand_expression env e1, expand_expression env e2)
| Bic_ISIRI e ->
    Bic_ISIRI (expand_expression env e)
| Bic_ISURI e ->
    Bic_ISURI (expand_expression env e)
| Bic_ISBLANK e ->
    Bic_ISBLANK (expand_expression env e)
| Bic_ISLITERAL e ->
    Bic_ISLITERAL (expand_expression env e)
| Bic_ISNUMERIC e ->
    Bic_ISNUMERIC (expand_expression env e)
| Bic_REGEXP (e1, e2, eopt) ->
    Bic_REGEXP
      (expand_expression env e1,
       expand_expression env e2,
       map_opt (expand_expression env) eopt
      )
| Bic_EXISTS g ->
    Bic_EXISTS (expand_group_graph_pattern env g)
| Bic_NOTEXISTS g ->
    Bic_NOTEXISTS (expand_group_graph_pattern env g)

and expand_group_var env t =
  { grpvar_loc = t.grpvar_loc ;
    grpvar_expr = map_opt (expand_expression env) t.grpvar_expr ;
    grpvar = t.grpvar ;
  }

and expand_group_condition env = function
  | GroupBuiltInCall c -> GroupBuiltInCall (expand_built_in_call env c)
  | GroupFunctionCall c -> GroupFunctionCall (expand_function_call env c)
  | GroupVar gv -> GroupVar (expand_group_var env gv)

and expand_constraint env = function
  | ConstrBuiltInCall c -> ConstrBuiltInCall (expand_built_in_call env c)
  | ConstrFunctionCall c -> ConstrFunctionCall (expand_function_call env c)
  | ConstrExpr e -> ConstrExpr (expand_expression env e)

and expand_having_condition = expand_constraint

and expand_order_condition env = function
  | OrderAsc e -> OrderAsc (expand_expression env e)
  | OrderDesc e -> OrderDesc (expand_expression env e)
  | OrderConstr c -> OrderConstr (expand_constraint env c)
  | OrderVar v -> OrderVar v

and expand_limit_offset_clause env t =
  { limoff_loc = t.limoff_loc ;
    limoff_offset = t.limoff_offset ;
    limoff_limit = t.limoff_limit ;
  }

and expand_solution_modifier env t =
  { solmod_loc = t.solmod_loc ;
    solmod_group = List.map (expand_group_condition env) t.solmod_group ;
    solmod_having = List.map (expand_having_condition env) t.solmod_having ;
    solmod_order = map_opt (List.map (expand_order_condition env)) t.solmod_order ;
    solmod_limoff = map_opt (expand_limit_offset_clause env) t.solmod_limoff ;
  }

and expand_bind env t =
  { bind_loc = t.bind_loc ;
    bind_expr = expand_expression env t.bind_expr ;
    bind_var = t.bind_var ;
  }

and expand_service_graph_pattern env t =
  { servgp_loc = t.servgp_loc ;
    servgp_silent = t.servgp_silent ;
    servgp_name = expand_var_or_iri env t.servgp_name ;
    servgp_pat = expand_group_graph_pattern env t.servgp_pat ;
  }

and expand_graph_graph_pattern env t =
  { graphgp_loc = t.graphgp_loc ;
    graphgp_name = expand_var_or_iri env t.graphgp_name ;
    graphgp_pat = expand_group_graph_pattern env t.graphgp_pat ;
  }

and expand_graph_pattern_not_triples env = function
  | Union l -> Union (List.map (expand_group_graph_pattern env) l)
  | Optional g -> Optional (expand_group_graph_pattern env g)
  | Minus g -> Minus (expand_group_graph_pattern env g)
  | GGP g -> GGP (expand_graph_graph_pattern env g)
  | Service s -> Service (expand_service_graph_pattern env s)
  | Filter c -> Filter (expand_constraint env c)
  | Bind b -> Bind (expand_bind env b)
  | InlineData d -> InlineData (expand_datablock env d)

and expand_graph_term env = function
  | GraphTermIri iri -> GraphTermIri (expand_iri env iri)
  | GraphTermLit lit -> GraphTermLit (expand_rdf_literal env lit)
  | GraphTermNumeric lit -> GraphTermNumeric (expand_rdf_literal env lit)
  | GraphTermBoolean lit -> GraphTermBoolean (expand_rdf_literal env lit)
  | GraphTermBlank b -> GraphTermBlank b
  | GraphTermNil -> GraphTermNil

and expand_var_or_term env = function
  | Var v -> Var v
  | GraphTerm t -> GraphTerm (expand_graph_term env t)

and expand_path_one_in_prop_set env = function
  | PathOneInIri iri -> PathOneInIri (expand_iri env iri)
  | PathOneInA -> PathOneInA iriref_a
  | PathOneInNotIri iri -> PathOneInNotIri (expand_iri env iri)
  | PathOneInNotA -> PathOneInNotIri iriref_a

and expand_path_primary env = function
  | PathIri iri -> PathIri (expand_iri env iri)
  | PathA -> PathIri iriref_a
  | PathNegPropSet l ->
      PathNegPropSet (List.map (expand_path_one_in_prop_set env) l)
  | Path p -> Path (expand_path env p)

and expand_path_elt env t = {
    pelt_loc = t.pelt_loc ;
    pelt_primary = expand_path_primary env t.pelt_primary ;
    pelt_mod = t.pelt_mod ;
  }

and expand_path_elt_or_inverse env = function
  | Elt e -> Elt (expand_path_elt env e)
  | Inv e -> Inv (expand_path_elt env e)

and expand_path_alternative env l =
  List.map (expand_path_elt_or_inverse env) l

and expand_path env l = List.map (expand_path_alternative env) l

and expand_verb_path env = function
  | VerbPath p -> VerbPath (expand_path env p)
  | VerbSimple v -> VerbSimple v

and expand_verb env = function
  | VerbVar v -> VerbVar v
  | VerbIri iri -> VerbIri (expand_iri env iri)
  | VerbA -> VerbIri iriref_a

and expand_triples_node env = function
  | TNodeCollection l ->
      TNodeCollection (List.map (expand_graph_node env) l)
  | TNodeBlank l ->
      TNodeBlank (List.map (expand_prop_object_list expand_verb env) l)

and expand_graph_node env = function
  | GraphNodeVT t -> GraphNodeVT (expand_var_or_term env t)
  | GraphNodeTriples t -> GraphNodeTriples (expand_triples_node env t)

and expand_triples_node_path env = function
  | TNodePathCollection l ->
      TNodePathCollection (List.map (expand_graph_node_path env) l)
  | TNodePathBlank p ->
      TNodePathBlank (expand_property_list_path env p)

and expand_graph_node_path env = function
  | GraphNodePathVT t -> GraphNodePathVT (expand_var_or_term env t)
  | GraphNodePathTriples t -> GraphNodePathTriples (expand_triples_node_path env t)

and expand_object_ = expand_graph_node
and expand_object_path = expand_graph_node_path

and expand_prop_object_list f env t =
  { propol_loc = t.propol_loc ;
    propol_verb = f env t.propol_verb ;
    propol_objects = List.map (expand_object env) t.propol_objects ;
  }

and expand_property_list_path env t =
  { proplp_loc = t.proplp_loc ;
    proplp_verb = expand_verb_path env t.proplp_verb ;
    proplp_objects = List.map (expand_object_path env) t.proplp_objects ;
    proplp_more = List.map
      (expand_prop_object_list expand_verb_path env) t.proplp_more ;
  }

and expand_triples_var_or_term_props f env t =
  { tvtp_loc = t.tvtp_loc ;
    tvtp_subject = expand_var_or_term env t.tvtp_subject ;
    tvtp_path = f env t.tvtp_path ;
  }

and expand_triples_node_path_props env t =
  { tnpp_loc = t.tnpp_loc ;
    tnpp_path = expand_triples_node_path env t.tnpp_path ;
    tnpp_props = map_opt (expand_property_list_path env) t.tnpp_props ;
  }

and expand_triples_same_subject_path env = function
  | TriplesPathVar t ->
      TriplesPathVar
        (expand_triples_var_or_term_props expand_property_path_list env t)
  | TriplesNodePath t ->
      TriplesNodePath (expand_triples_node_path_props env t)

and expand_triples_block env t =
  { triples_loc = t.triples_loc ;
    triples = List.map (expand_triples_same_subject_path env) t.triples ;
  }

and expand_triples_node_props env t =
  { tnp_loc = t.tnp_loc ;
    tnp_path = expand_triples_node env t.tnp_path ;
    tnp_props = List.map (expand_prop_object_list expand_verb env) t.tnp_props ;
  }

and expand_ggp_sub =
  let f_rest env (graph_pattern_not_triples, triples_block_option) =
      (expand_graph_pattern_not_triples env graph_pattern_not_triples,
       map_opt (expand_triples_block_option env) triples_block_option
      )
  in
  fun env t ->
      {
        ggp_sub_loc = t.ggp_sub_loc ;
        ggp_sub_triples = map_opt (expand_triples_block env) t.ggp_sub_triples ;
        ggp_sub_rest = List.map (f_rest env) t.ggp_sub_rest ;
      }

and expand_group_graph_pattern env = function
  | SubSelect s -> expand_sub_select env s
  | GGPSub g -> expand_ggp_sub env g

and expand_sub_select env t =
  { subsel_loc = t.subsel_loc ;
    subsel_select = expand_select_clause env t.subsel_select ;
    subsel_where = expand_group_graph_pattern env t.subsel_where ;
    subsel_modifier = expand_solution_modifier env t.subsel_modifier ;
    subsel_values = expand_values_clause env t.subsel_values ;
  }
;;

let expand_select_query env t =
  {
    select_select = expand_select_clause env t.select_select ;
    select_dataset = List.map (expand_dataset_clause env) t.select_dataset ;
    select_where = expand_group_graph_pattern env t.select_where ;
    select_modifier = expand_solution_modifier env t.select_modifier ;
  }

let expand_triples_same_subject env = function
  | TriplesVar t ->
    TriplesVar
      (expand_triples_var_or_term_props
       (fun env l -> List.map (expand_prop_object_list expand_verb env) l)
         env t
      )
  | TriplesNode t -> TriplesNode (expand_triples_node_props env t)

let triples_template env l = List.map (triples_same_subject env) l
let expand_construct_template = expand_triples_template

let expand_construct_where env = function
  | Constr_ggp p -> Constr_ggp (expand_group_graph_pattern env p)
  | Constr_template t -> Constr_template (expand_triples_template env t)

let expand_construct_query env t =
  {
    constr_template = map_opt (expand_construct_template env) t.constr_template ;
    constr_dataset = List.map (expand_dataset_clause env) t.constr_dataset ;
    constr_where = expand_construct_where env t.constr_where ;
    constr_modifier = expand_solution_modifier t.constr_modifier ;
  }

let expand_describe_query env t =
  {
    desc_sel = List.map (expand_var_or_iri env) t.desc_sel ;
    desc_dataset = List.map (expand_dataset_clause env) t.desc_dataset ;
    desc_where = map_opt (expand_group_graph_pattern env) t.desc_where ;
    desc_modifier = expand_solution_modifier env t.desc_modifier;
  }

let expand_ask_query env t =
  {
    ask_dataset = List.map (expand_dataset_clause env) t.ask_dataset ;
    ask_where = expand_group_graph_pattern env t.ask_where;
    ask_modifier = expand_solution_modifier env t.ask_modifier;
  }

let expand_query_kind env = function
  | Select q -> Select (expand_select_query env q)
  | Construct q -> Construct (expand_construct_query env q)
  | Describe q -> Describe (expand_describe_query env q)
  | Ask q -> Ask (expand_ask_query env q)
;;


let expand_query q =
  let env = () in
  let (q_prolog, env) = expand_prolog env q.q_prolog in
  let q_kind = expand_kind env q.q_kind in
  let q_values = expand_values env q.q_values in
  { q_prolog ; q_kind ; q_values }
;;
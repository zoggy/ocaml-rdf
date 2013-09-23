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

(** *)

open Rdf_sparql_types
module T = Rdf_sparql_types

let map_opt = Rdf_misc.map_opt

type ('acc, 't) map_fun = 'acc mapper -> 'acc -> 't -> 't

and 'a mapper =
  {
    var : ('a, var) map_fun ;
    iriref : ('a, iriref) map_fun ;
    prefixed_name : ('a, prefixed_name) map_fun ;
    reliri : ('a, rel_iri) map_fun ;
    iri : ('a, iri) map_fun ;
    rdf_literal : ('a, rdf_literal) map_fun ;
    data_block_value : ('a, data_block_value) map_fun ;
    data_full_block_value : ('a, data_full_block_value ) map_fun ;
    inline_data_one_var : ('a, inline_data_one_var) map_fun ;
    inline_data_full : ('a, inline_data_full) map_fun ;
    datablock : ('a, datablock) map_fun ;
    values_clause : ('a, values_clause) map_fun ;
    var_or_iri : ('a, var_or_iri) map_fun ;
    blank_node : ('a, blank_node) map_fun ;
    select_var : ('a, select_var) map_fun ;
    select_vars : ('a, select_vars) map_fun ;
    select_clause : ('a, select_clause) map_fun ;
    dataset_clause : ('a, dataset_clause) map_fun ;
    arg_list : ('a, arg_list) map_fun ;
    function_call : ('a, function_call) map_fun ;
    binary_op : ('a, binary_op) map_fun ;
    expr : ('a, expr) map_fun ;
    expression : ('a, expression) map_fun ;
    built_in_call : ('a, built_in_call) map_fun ;
    aggregate : ('a, aggregate) map_fun ;
    group_var : ('a, group_var) map_fun ;
    group_condition : ('a, group_condition) map_fun ;
    constraint_ : ('a, constraint_) map_fun ;
    order_condition : ('a, order_condition) map_fun ;
    limit_offset_clause : ('a, limit_offset_clause) map_fun ;
    solution_modifier : ('a, solution_modifier) map_fun ;
    bind : ('a, bind) map_fun ;
    service_graph_pattern : ('a, service_graph_pattern) map_fun ;
    graph_graph_pattern : ('a, graph_graph_pattern) map_fun ;
    graph_pattern_elt : ('a, graph_pattern_elt) map_fun ;
    graph_term : ('a, graph_term) map_fun ;
    var_or_term : ('a, var_or_term) map_fun ;
    path_one_in_prop_set : ('a, path_one_in_prop_set) map_fun ;
    path_primary : ('a, path_primary) map_fun ;
    path_elt : ('a, path_elt) map_fun ;
    path_elt_or_inverse : ('a, path_elt_or_inverse) map_fun ;
    path_sequence : ('a, path_sequence) map_fun ;
    path : ('a, path) map_fun ;
    verb : ('a, verb) map_fun ;
    triples_node : ('a, triples_node) map_fun ;
    graph_node : ('a, graph_node) map_fun ;
    prop_object_list : ('a, prop_object_list) map_fun ;
    triples_block : ('a, triples_block) map_fun ;
    triples_same_subject : ('a, triples_same_subject) map_fun ;
    ggp_sub : ('a, ggp_sub) map_fun ;
    group_graph_pattern : ('a, group_graph_pattern) map_fun ;
    sub_select : ('a, sub_select) map_fun ;
  }

let var f acc t = t
let iriref f acc t = t
let prefixed_name f acc t = t
let reliri f acc t = t
let iri f acc = function
| Iriref i -> Iriref (f.iriref f acc i)
| PrefixedName p -> PrefixedName (f.prefixed_name f acc p)
| Reliri r -> Reliri (f.reliri f acc r)

let rdf_literal f acc t = t

let data_block_value f acc = function
  | DataBlockValueIri iri -> DataBlockValueIri (f.iri f acc iri)
  | DataBlockValueRdf lit -> DataBlockValueRdf (f.rdf_literal f acc lit)
  | DataBlockValueNumeric lit -> DataBlockValueNumeric (f.rdf_literal f acc lit)
  | DataBlockValueBoolean lit -> DataBlockValueBoolean (f.rdf_literal f acc lit)
  | DataBlockValueUndef -> DataBlockValueUndef
;;

let data_full_block_value f acc = function
  | Nil -> Nil
  | Value l -> Value (List.map (f.data_block_value f acc) l)
;;

let inline_data_one_var f acc t =
  { idov_loc = t.idov_loc ;
    idov_var = f.var f acc t.idov_var ;
    idov_data = List.map (f.data_block_value f acc) t.idov_data ;
  }

;;

let inline_data_full f acc t =
  { idf_loc = t.idf_loc ;
    idf_vars = List.map (f.var f acc) t.idf_vars ;
    idf_values = List.map (f.data_full_block_value f acc) t.idf_values ;
  }
;;

let datablock f acc = function
  | InLineDataOneVar d -> InLineDataOneVar (f.inline_data_one_var f acc d)
  | InLineDataFull d -> InLineDataFull (f.inline_data_full f acc d)
;;

let values_clause f acc = function None -> None | Some d -> Some (f.datablock f acc d);;

let var_or_iri f acc = function
  | VIVar var -> VIVar (f.var f acc var)
  | VIIri iri -> VIIri (f.iri f acc iri)
;;

let blank_node f acc t = t

let rec select_var f acc t =
  { sel_var_loc = t.sel_var_loc ;
    sel_var_expr = map_opt (f.expression f acc) t.sel_var_expr ;
    sel_var = f.var f acc t.sel_var ;
  }

and select_vars f acc = function
  | SelectAll -> SelectAll
  | SelectVars l -> SelectVars (List.map (f.select_var f acc) l)

and select_clause f acc t =
  { sel_flag = t.sel_flag ;
    sel_vars = f.select_vars f acc t.sel_vars;
  }

and dataset_clause f acc = function
  | DefaultGraphClause s -> DefaultGraphClause (f.iri f acc s)
  | NamedGraphClause s -> NamedGraphClause (f.iri f acc s)

and arg_list f acc t =
  { argl_loc = t.argl_loc ;
    argl_distinct = t.argl_distinct ;
    argl = List.map (f.expression f acc) t.argl ;
  }

and function_call f acc t =
  { func_loc = t.func_loc ;
    func_iri = f.iri f acc t.func_iri ;
    func_args = f.arg_list f acc t.T.func_args ;
  }

and binary_op f acc t = t

and expr f acc = function
  | EVar var -> EVar (f.var f acc var)
  | EIri iri -> EIri (f.iri f acc iri)
  | EBin (e1, op, e2) ->
      EBin
         (f.expression f acc e1,
          f.binary_op f acc op,
          f.expression f acc e2
         )
  | ENot e -> ENot (f.expression f acc e)
  | EUMinus e -> EUMinus (f.expression f acc e)
  | EBic c -> EBic (f.built_in_call f acc c)
  | EFuncall c -> EFuncall (f.function_call f acc c)
  | ELit lit -> ELit (f.rdf_literal f acc lit)
  | ENumeric lit -> ENumeric (f.rdf_literal f acc lit)
  | EBoolean lit -> EBoolean (f.rdf_literal f acc lit)
  | EIn (e, l) ->
         EIn (f.expression f acc e, List.map (f.expression f acc) l)
  | ENotIn (e, l) ->
         ENotIn (f.expression f acc e, List.map (f.expression f acc) l)

and expression f acc t =
  { expr_loc = t.expr_loc ;
    T.expr = f.expr f acc t.T.expr ;
  }

and built_in_call f acc = function
  | Bic_agg agg -> Bic_agg (f.aggregate f acc agg)
  | Bic_fun (name, l) ->
      Bic_fun (name, List.map (f.expression f acc) l)
  | Bic_BOUND v -> Bic_BOUND (f.var f acc v)
  | Bic_EXISTS ggp -> Bic_EXISTS (f.group_graph_pattern f acc ggp)
  | Bic_NOTEXISTS ggp -> Bic_NOTEXISTS (f.group_graph_pattern f acc ggp)

and aggregate f acc = function
  | Bic_COUNT (d, e) -> Bic_COUNT (d, map_opt (f.expression f acc) e)
  | Bic_SUM (d, e) -> Bic_SUM (d, f.expression f acc e)
  | Bic_MIN (d, e) -> Bic_MIN (d, f.expression f acc e)
  | Bic_MAX (d, e) -> Bic_MAX (d, f.expression f acc e)
  | Bic_AVG (d, e) -> Bic_AVG (d, f.expression f acc e)
  | Bic_SAMPLE (d, e) -> Bic_SAMPLE (d, f.expression f acc e)
  | Bic_GROUP_CONCAT (d, e, sopt) -> Bic_GROUP_CONCAT (d, f.expression f acc e, sopt)

and group_var f acc t =
  { grpvar_loc = t.grpvar_loc ;
    grpvar_expr = map_opt (f.expression f acc) t.grpvar_expr ;
    grpvar = map_opt (f.var f acc) t.grpvar ;
  }

and group_condition f acc = function
  | GroupBuiltInCall c -> GroupBuiltInCall (f.built_in_call f acc c)
  | GroupFunctionCall c -> GroupFunctionCall (f.function_call f acc c)
  | GroupVar gv -> GroupVar (f.group_var f acc gv)

and constraint_ f acc = function
  | ConstrBuiltInCall c -> ConstrBuiltInCall (f.built_in_call f acc c)
  | ConstrFunctionCall c -> ConstrFunctionCall (f.function_call f acc c)
  | ConstrExpr e -> ConstrExpr (f.expression f acc e)

and order_condition f acc = function
  | OrderAsc e -> OrderAsc (f.expression f acc e)
  | OrderDesc e -> OrderDesc (f.expression f acc e)
  | OrderConstr c -> OrderConstr (f.constraint_ f acc c)
  | OrderVar v -> OrderVar (f.var f acc v)

and limit_offset_clause f acc t = t

and solution_modifier f acc t =
  { solmod_loc = t.solmod_loc ;
    solmod_group = List.map (f.group_condition f acc) t.solmod_group ;
    solmod_having = List.map (f.constraint_ f acc) t.solmod_having ;
    solmod_order = map_opt (List.map (f.order_condition f acc)) t.solmod_order ;
    solmod_limoff = map_opt (f.limit_offset_clause f acc) t.solmod_limoff ;
  }

and bind f acc t =
  { bind_loc = t.bind_loc ;
    bind_expr = f.expression f acc t.bind_expr ;
    bind_var = f.var f acc t.bind_var ;
  }

and service_graph_pattern f acc t =
  { servgp_loc = t.servgp_loc ;
    servgp_silent = t.servgp_silent ;
    servgp_name = f.var_or_iri f acc t.servgp_name ;
    servgp_pat = f.group_graph_pattern f acc t.servgp_pat ;
  }

and graph_graph_pattern f acc t =
  { graphgp_loc = t.graphgp_loc ;
    graphgp_name = f.var_or_iri f acc t.graphgp_name ;
    graphgp_pat = f.group_graph_pattern f acc t.graphgp_pat ;
  }

and graph_pattern_elt f acc = function
  | Triples t -> Triples (f.triples_block f acc t)
  | Union l -> Union (List.map (f.group_graph_pattern f acc) l)
  | Optional ggp -> Optional (f.group_graph_pattern f acc ggp)
  | Minus ggp -> Minus (f.group_graph_pattern f acc ggp)
  | GGP g -> GGP (f.graph_graph_pattern f acc g)
  | Service p -> Service (f.service_graph_pattern f acc p)
  | Filter c -> Filter (f.constraint_ f acc c)
  | Bind b -> Bind (f.bind f acc b)
  | InlineData d -> InlineData (f.datablock f acc d)

and graph_term f acc = function
  | GraphTermIri iri -> GraphTermIri (f.iri f acc iri)
  | GraphTermLit lit -> GraphTermLit (f.rdf_literal f acc lit)
  | GraphTermNumeric lit -> GraphTermNumeric (f.rdf_literal f acc lit)
  | GraphTermBoolean lit -> GraphTermBoolean (f.rdf_literal f acc lit)
  | GraphTermBlank b -> GraphTermBlank (f.blank_node f acc b)
  | GraphTermNil -> GraphTermNil
  | GraphTermNode _ -> assert false

and var_or_term f acc = function
  | Var v -> Var (f.var f acc v)
  | GraphTerm t -> GraphTerm (f.graph_term f acc t)

and path_one_in_prop_set f acc = function
  | PathOneInIri iri -> PathOneInIri (f.iri f acc iri)
  | PathOneInA -> PathOneInA
  | PathOneInNotIri iri -> PathOneInNotIri (f.iri f acc iri)
  | PathOneInNotA -> PathOneInNotA

and path_primary f acc = function
  | PathIri iri -> PathIri (f.iri f acc iri)
  | PathA -> PathA
  | PathNegPropSet l -> PathNegPropSet (List.map (f.path_one_in_prop_set f acc) l)
  | Path p -> Path (f.path f acc p)

and path_elt f acc t =
  { pelt_loc = t.pelt_loc ;
    pelt_mod = t.pelt_mod ;
    pelt_primary = f.path_primary f acc t.pelt_primary ;
  }

and path_elt_or_inverse f acc = function
  | Elt p -> Elt (f.path_elt f acc p)
  | Inv p -> Inv (f.path_elt f acc p)

and path_sequence f acc l = List.map (f.path_elt_or_inverse f acc) l

and path f acc l = List.map (f.path_sequence f acc) l

and verb f acc = function
  | VerbPath p -> VerbPath (f.path f acc p)
  | VerbVar v -> VerbVar (f.var f acc v)
  | VerbIri i -> VerbIri (f.iri f acc i)
  | VerbA -> VerbA

and triples_node f acc = function
  | TNodeCollection l -> TNodeCollection (List.map (f.graph_node f acc) l)
  | TNodeBlank l -> TNodeBlank (List.map (f.prop_object_list f acc) l)

and graph_node f acc = function
  | GraphNodeVT t -> GraphNodeVT (f.var_or_term f acc t)
  | GraphNodeTriples t -> GraphNodeTriples (f.triples_node f acc t)

and prop_object_list f acc t =
  { propol_loc = t.propol_loc ;
    propol_verb = f.verb f acc t.propol_verb ;
    propol_objects = List.map (f.graph_node f acc) t.propol_objects ;
  }

and triples_block f acc t =
  { triples_loc = t.triples_loc ;
    triples = List.map (f.triples_same_subject f acc) t.triples ;
  }

and triples_same_subject f acc = function
  | TriplesVar (t, l) ->
      TriplesVar
        (f.var_or_term f acc t,
         List.map (f.prop_object_list f acc) l
        )
  | TriplesNode (t, l) ->
      TriplesNode
        (f.triples_node f acc t,
         List.map (f.prop_object_list f acc) l
        )

and ggp_sub f acc t =
  { ggp_sub_loc = t.ggp_sub_loc ;
    ggp_sub_elts = List.map (f.graph_pattern_elt f acc) t.ggp_sub_elts ;
  }

and group_graph_pattern f acc = function
  | SubSelect s -> SubSelect (f.sub_select f acc s)
  | GGPSub g -> GGPSub (f.ggp_sub f acc g)

and sub_select f acc t =
  { subsel_loc = t.subsel_loc ;
    subsel_select = f.select_clause f acc t.subsel_select ;
    subsel_where = f.group_graph_pattern f acc t.subsel_where ;
    subsel_modifier = f.solution_modifier f acc t.subsel_modifier ;
    subsel_values = f.values_clause f acc t.subsel_values ;
  }

let default = {
    var ;
    iriref ;
    prefixed_name ;
    reliri ;
    iri ;
    rdf_literal ;
    data_block_value ;
    data_full_block_value ;
    inline_data_one_var ;
    inline_data_full ;
    datablock ;
    values_clause ;
    var_or_iri ;
    blank_node ;
    select_var ;
    select_vars ;
    select_clause ;
    dataset_clause ;
    arg_list ;
    function_call ;
    binary_op ;
    expr ;
    expression ;
    built_in_call ;
    aggregate ;
    group_var ;
    group_condition ;
    constraint_ ;
    order_condition ;
    limit_offset_clause ;
    solution_modifier ;
    bind ;
    service_graph_pattern ;
    graph_graph_pattern ;
    graph_pattern_elt ;
    graph_term ;
    var_or_term ;
    path_one_in_prop_set ;
    path_primary ;
    path_elt ;
    path_elt_or_inverse ;
    path_sequence ;
    path ;
    verb ;
    triples_node ;
    graph_node ;
    prop_object_list ;
    triples_block ;
    triples_same_subject ;
    ggp_sub ;
    group_graph_pattern ;
    sub_select ;
}



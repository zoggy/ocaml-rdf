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

(** *)

open Rdf_sparql_types
module T = Rdf_sparql_types

let map_opt f def = function
  | None -> def
  | Some x -> f def x

type ('acc, 't) visitor_fun = 'acc visitor -> 'acc -> 't -> 'acc

and 'a visitor =
  {
    var : ('a, var) visitor_fun ;
    iriref : ('a, iriref) visitor_fun ;
    prefixed_name : ('a, prefixed_name) visitor_fun ;
    iriloc : ('a, iriloc) visitor_fun ;
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

let var f acc t = acc
let iriref f acc t = acc
let prefixed_name f acc t = acc
let iriloc f acc t = acc

let iri f acc = function
  | Iriref i -> f.iriref f acc i
  | PrefixedName p -> f.prefixed_name f acc p
  | Iri r -> f.iriloc f acc r

let rdf_literal f acc t = acc

let data_block_value f acc = function
  | DataBlockValueIri iri -> f.iri f acc iri
  | DataBlockValueRdf lit
  | DataBlockValueNumeric lit
  | DataBlockValueBoolean lit -> f.rdf_literal f acc lit
  | DataBlockValueUndef -> acc
;;

let data_full_block_value f acc = function
  | Nil -> acc
  | Value l -> List.fold_left (f.data_block_value f) acc l
;;

let inline_data_one_var f acc t =
  let acc = (f.var f acc t.idov_var) in
  List.fold_left (f.data_block_value f) acc t.idov_data
;;

let inline_data_full f acc t =
  let acc = List.fold_left (f.var f) acc t.idf_vars in
  List.fold_left (f.data_full_block_value f) acc t.idf_values
;;

let datablock f acc = function
  | InLineDataOneVar d -> f.inline_data_one_var f acc d
  | InLineDataFull d -> f.inline_data_full f acc d
;;

let values_clause f acc = function None -> acc | Some d -> f.datablock f acc d;;

let var_or_iri f acc = function
  | VIVar var -> f.var f acc var
  | VIIri iri -> f.iri f acc iri
;;

let blank_node f acc t = acc;;

let rec select_var f acc t =
  let acc = match t.sel_var_expr with
      None -> acc
    | Some e -> f.expression f acc e
  in
  f.var f acc t.sel_var

and select_vars f acc = function
  | SelectAll -> acc
  | SelectVars l -> List.fold_left (f.select_var f) acc l

and select_clause f acc t = f.select_vars f acc t.sel_vars

and dataset_clause f acc = function
  | DefaultGraphClause s
  | NamedGraphClause s -> f.iri f acc s

and arg_list f acc t = List.fold_left (f.expression f) acc t.argl

and function_call f acc t =
  let acc = f.iri f acc t.func_iri in
  f.arg_list f acc t.T.func_args

and binary_op f acc _ = acc

and expr f acc = function
  | EVar var -> f.var f acc var
  | EIri iri -> f.iri f acc iri
  | EBin (e1, _, e2) ->
      let acc = f.expression f acc e1 in
      f.expression f acc e2
  | ENot e
  | EUMinus e -> f.expression f acc e
  | EBic c -> f.built_in_call f acc c
  | EFuncall c -> f.function_call f acc c
  | ELit lit
  | ENumeric lit
  | EBoolean lit -> f.rdf_literal f acc lit
  | EIn (e, l)
  | ENotIn (e, l) -> List.fold_left (f.expression f) acc (e::l)

and expression f acc t = f.expr f acc t.T.expr

and built_in_call f acc = function
  | Bic_agg agg -> f.aggregate f acc agg
  | Bic_fun (_, l) -> List.fold_left (f.expression f) acc l
  | Bic_BOUND v -> f.var f acc v
  | Bic_EXISTS ggp
  | Bic_NOTEXISTS ggp -> f.group_graph_pattern f acc ggp

and aggregate f acc = function
  | Bic_COUNT (_, None) -> acc
  | Bic_COUNT (_, Some e)
  | Bic_SUM (_, e)
  | Bic_MIN (_, e)
  | Bic_MAX (_, e)
  | Bic_AVG (_, e)
  | Bic_SAMPLE (_, e)
  | Bic_GROUP_CONCAT (_, e, _) -> f.expression f acc e

and group_var f acc t =
  let acc = map_opt (f.expression f) acc t.grpvar_expr in
  map_opt (f.var f) acc t.grpvar

and group_condition f acc = function
  | GroupBuiltInCall c -> f.built_in_call f acc c
  | GroupFunctionCall c -> f.function_call f acc c
  | GroupVar gv -> f.group_var f acc gv

and constraint_ f acc = function
  | ConstrBuiltInCall c -> f.built_in_call f acc c
  | ConstrFunctionCall c -> f.function_call f acc c
  | ConstrExpr e -> f.expression f acc e

and order_condition f acc = function
  | OrderAsc e
  | OrderDesc e -> f.expression f acc e
  | OrderConstr c -> f.constraint_ f acc c
  | OrderVar v -> f.var f acc v

and limit_offset_clause f acc t = acc

and solution_modifier f acc t =
  let acc = List.fold_left (f.group_condition f) acc t.solmod_group in
  let acc = List.fold_left (f.constraint_ f) acc t.solmod_having in
  let acc = map_opt (fun acc l -> List.fold_left (f.order_condition f) acc l) acc t.solmod_order in
  map_opt (f.limit_offset_clause f) acc t.solmod_limoff

and bind f acc t =
  let acc = f.expression f acc t.bind_expr in
  f.var f acc t.bind_var

and service_graph_pattern f acc t =
  let acc = f.var_or_iri f acc t.servgp_name in
  f.group_graph_pattern f acc t.servgp_pat

and graph_graph_pattern f acc t =
  let acc = f.var_or_iri f acc t.graphgp_name in
  f.group_graph_pattern f acc t.graphgp_pat

and graph_pattern_elt f acc = function
  | Triples t -> f.triples_block f acc t
  | Union l -> List.fold_left (f.group_graph_pattern f) acc l
  | Optional ggp
  | Minus ggp -> f.group_graph_pattern f acc ggp
  | GGP g -> f.graph_graph_pattern f acc g
  | Service p -> f.service_graph_pattern f acc p
  | Filter c -> f.constraint_ f acc c
  | Bind b -> f.bind f acc b
  | InlineData d -> f.datablock f acc d

and graph_term f acc = function
  | GraphTermIri iri -> f.iri f acc iri
  | GraphTermLit lit
  | GraphTermNumeric lit
  | GraphTermBoolean lit -> f.rdf_literal f acc lit
  | GraphTermBlank b -> f.blank_node f acc b
  | GraphTermNil -> acc
  | GraphTermNode _ -> assert false

and var_or_term f acc = function
  | Var v -> f.var f acc v
  | GraphTerm t -> f.graph_term f acc t

and path_one_in_prop_set f acc = function
  | PathOneInIri iri -> f.iri f acc iri
  | PathOneInA -> acc
  | PathOneInNotIri iri -> f.iri f acc iri
  | PathOneInNotA -> acc

and path_primary f acc = function
  | PathIri iri -> f.iri f acc iri
  | PathA -> acc
  | PathNegPropSet l -> List.fold_left (f.path_one_in_prop_set f) acc l
  | Path p -> f.path f acc p

and path_elt f acc t = f.path_primary f acc t.pelt_primary

and path_elt_or_inverse f acc = function
  | Elt p
  | Inv p -> f.path_elt f acc p

and path_sequence f acc l = List.fold_left (f.path_elt_or_inverse f) acc l

and path f acc l = List.fold_left (f.path_sequence f) acc l

and verb f acc = function
  | VerbPath p -> f.path f acc p
  | VerbVar v -> f.var f acc v
  | VerbIri i -> f.iri f acc i
  | VerbA -> acc

and triples_node f acc = function
  | TNodeCollection l -> List.fold_left (f.graph_node f) acc l
  | TNodeBlank l -> List.fold_left (f.prop_object_list f) acc l

and graph_node f acc = function
  | GraphNodeVT t -> f.var_or_term f acc t
  | GraphNodeTriples t -> f.triples_node f acc t

and prop_object_list f acc t =
      let acc = f.verb f acc t.propol_verb in
      List.fold_left (f.graph_node f) acc t.propol_objects

and triples_block f acc t =
 List.fold_left (f.triples_same_subject f) acc t.triples

and triples_same_subject f acc = function
  | TriplesVar (t, l) ->
    let acc = f.var_or_term f acc t in
    List.fold_left (f.prop_object_list f) acc l
  | TriplesNode (t, l) ->
      let acc = f.triples_node f acc t in
      List.fold_left (f.prop_object_list f) acc l

and ggp_sub f acc t = List.fold_left (f.graph_pattern_elt f) acc t.ggp_sub_elts

and group_graph_pattern f acc = function
  | SubSelect s -> f.sub_select f acc s
  | GGPSub g -> f.ggp_sub f acc g

and sub_select f acc t =
  let acc = f.select_clause f acc t.subsel_select in
  let acc = f.group_graph_pattern f acc t.subsel_where in
  let acc = f.solution_modifier f acc t.subsel_modifier in
  f.values_clause f acc t.subsel_values

let default = {
    var ;
    iriref ;
    prefixed_name ;
    iriloc ;
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



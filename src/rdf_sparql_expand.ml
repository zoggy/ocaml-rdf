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

(** Expanding IRIs and triples in query abstract syntax tree. *)

open Rdf_sparql_types

let map_opt = Rdf_misc.map_opt;;

module SMap = Rdf_xml.SMap;;

type env =
  { base : Rdf_iri.iri ;
    prefixes : Rdf_iri.iri SMap.t ;
  }

type dataset = { from : Rdf_iri.iri option ; from_named : Rdf_iri.Iriset.t }

let create_env base = { base ; prefixes = SMap.empty }

let iriref_a =
  Iriref
    { ir_loc = Rdf_loc.dummy_loc ;
      ir_iri = Rdf_rdf.rdf_type ;
    }
;;

let expand_iri env = function
| Iriref ir -> Iriref ir
| Reliri r ->
    Iriref {
      ir_iri = Rdf_iri.ensure_absolute env.base r.reliri ;
      ir_loc = r.reliri_loc ;
    }
| PrefixedName pname ->
    let base =
      match pname.pname_ns.pname_ns_name with
        "" -> env.base
      | s ->
          try SMap.find s env.prefixes
          with Not_found ->
              Rdf_sparql_algebra.error (Rdf_sparql_algebra.Unknown_prefix pname.pname_ns)
    in
    let iri =
      match pname.pname_local with
        None -> base
      | Some l ->
          let s = (Rdf_iri.string base ^ l.pname_local_name) in
          Rdf_iri.iri s
    in
    Iriref { ir_loc = pname.pname_loc ; ir_iri = iri }
;;

let expand_query_prolog_decl (env, acc) = function
| (BaseDecl iriref) as t
| (PrefixDecl ({ pname_ns_name = "" }, iriref) as t) ->
    let env = { env with base = iriref.ir_iri } in
    (env, t :: acc)

| PrefixDecl (pname_ns, iriref) as t->
    let prefixes = SMap.add pname_ns.pname_ns_name iriref.ir_iri env.prefixes in
    ({ env with prefixes }, t :: acc)
;;

let expand_query_prolog env decls =
  let (env, l) = List.fold_left expand_query_prolog_decl (env, []) decls in
  (env, List.rev l)
;;

let expand_rdf_literal env t =
  match t.rdf_lit_type with
    None -> t
  | Some iri ->
      match expand_iri env iri with
        PrefixedName _
      | Reliri _  -> assert false
      | Iriref i ->
          { rdf_lit_loc = t.rdf_lit_loc ;
            rdf_lit = { t.rdf_lit with Rdf_term.lit_type = Some i.ir_iri } ;
            rdf_lit_type = Some (Iriref i) ;
          }
;;

let expand_data_block_value env = function
  | DataBlockValueIri iri -> DataBlockValueIri (expand_iri env iri)
  | DataBlockValueRdf lit -> DataBlockValueRdf (expand_rdf_literal env lit)
  | DataBlockValueNumeric lit -> DataBlockValueNumeric (expand_rdf_literal env lit)
  | DataBlockValueBoolean lit -> DataBlockValueBoolean (expand_rdf_literal env lit)
  | DataBlockValueUndef -> DataBlockValueUndef
;;

let expand_data_full_block_value env = function
  | Nil -> Nil
  | Value l -> Value (List.map (expand_data_block_value env) l)
;;

let expand_inline_data_one_var env t =
  { idov_loc = t.idov_loc ;
    idov_var = t.idov_var ;
    idov_data = List.map (expand_data_block_value env) t.idov_data ;
  }
;;

let expand_inline_data_full env t =
  { idf_loc = t.idf_loc ;
    idf_vars = t.idf_vars ;
    idf_values = List.map (expand_data_full_block_value env) t.idf_values ;
  }
;;

let expand_datablock env = function
  | InLineDataOneVar i -> InLineDataOneVar (expand_inline_data_one_var env i)
  | InLineDataFull i -> InLineDataFull (expand_inline_data_full env i)

let expand_values_clause env d = map_opt (expand_datablock env) d;;

let expand_var_or_iri env = function
  | VIVar v -> VIVar v
  | VIIri iri -> VIIri (expand_iri env iri)
;;


let rec expand_select_var env t =
  { sel_var_loc = t.sel_var_loc ;
    sel_var_expr = map_opt (expand_expression env) t.sel_var_expr ;
    sel_var = t.sel_var ;
  }

and expand_select_vars env = function
  | SelectAll -> SelectAll
  | SelectVars l -> SelectVars (List.map (expand_select_var env) l)

and expand_select_clause env t =
  {
    sel_flag = t.sel_flag ;
    sel_vars = expand_select_vars env t.sel_vars ;
  }

and expand_source_selector = expand_iri

and expand_dataset_clause env = function
  | DefaultGraphClause s ->
    DefaultGraphClause (expand_source_selector env s)
  | NamedGraphClause s ->
    NamedGraphClause (expand_source_selector env s)

and expand_arg_list env t  =
  { argl_loc = t.argl_loc ;
    argl_distinct = t.argl_distinct ;
    argl = List.map (expand_expression env) t.argl ;
  }
and expand_function_call env t =
  { func_loc = t.func_loc ;
    func_iri = expand_iri env t.func_iri ;
    func_args = expand_arg_list env t.func_args ;
  }

and expand_expr env = function
  | EIri iri -> EIri (expand_iri env iri)
  | EBin (e1, op, e2) ->
      EBin
        (expand_expression env e1,
         op,
         expand_expression env e2)
  | EIn (e, l) ->
      EIn
        (expand_expression env e,
         List.map (expand_expression env) l)
  | ENotIn (e, l) ->
      ENotIn
        (expand_expression env e,
         List.map (expand_expression env) l)
  | EUMinus e ->
      EUMinus (expand_expression env e)
  | ENot e -> ENot (expand_expression env e)
  | EBic c -> EBic (expand_built_in_call env c)
  | EFuncall c -> EFuncall (expand_function_call env c)
  | ELit lit -> ELit (expand_rdf_literal env lit)
  | ENumeric lit -> ENumeric (expand_rdf_literal env lit)
  | EBoolean lit -> EBoolean (expand_rdf_literal env lit)
  | EVar v -> EVar v

and expand_expression env t =
  { expr_loc = t.expr_loc ;
    expr = expand_expr env t.expr ;
  }

and expand_aggregate env = function
| Bic_COUNT (b, eopt) ->
    Bic_COUNT (b, map_opt (expand_expression env) eopt)
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

and expand_built_in_call env = function
  | Bic_agg agg -> Bic_agg (expand_aggregate env agg)
  | Bic_fun (name, l) -> Bic_fun (name, List.map (expand_expression env) l)
  | Bic_BOUND v -> Bic_BOUND v
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

and expand_having_condition env t = expand_constraint env t

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

and expand_graph_pattern_elt env = function
  | Triples l -> Triples (expand_triples_block env l)
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
  | GraphTermBlank ({ bnode_label = None } as b) ->
      (* set a fresh id, needed to distinguish this anonymous blank
        from others un solution union *)
      let label = Rdf_sparql_ms.gen_blank_id () in
      GraphTermBlank { b with bnode_label = Some label }
  | GraphTermBlank b -> GraphTermBlank b
  | GraphTermNil -> GraphTermNil
  | GraphTermNode _ -> assert false

and expand_var_or_term env = function
  | Var v -> Var v
  | GraphTerm t -> GraphTerm (expand_graph_term env t)

and expand_path_one_in_prop_set env = function
  | PathOneInIri iri -> PathOneInIri (expand_iri env iri)
  | PathOneInA -> PathOneInIri iriref_a
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

and expand_path_sequence env l =
  List.map (expand_path_elt_or_inverse env) l

and expand_path env l = List.map (expand_path_sequence env) l

and expand_verb env = function
  | VerbPath p -> VerbPath (expand_path env p)
  | VerbVar v -> VerbVar v
  | VerbIri iri -> VerbIri (expand_iri env iri)
  | VerbA -> VerbIri iriref_a

and expand_triples_node env = function
  | TNodeCollection l ->
      TNodeCollection (List.map (expand_graph_node env) l)
  | TNodeBlank l ->
      TNodeBlank (List.map (expand_prop_object_list env) l)

and expand_graph_node env = function
  | GraphNodeVT t -> GraphNodeVT (expand_var_or_term env t)
  | GraphNodeTriples t -> GraphNodeTriples (expand_triples_node env t)

and expand_object env t = expand_graph_node env t

and expand_prop_object_list env t =
  { propol_loc = t.propol_loc ;
    propol_verb = expand_verb env t.propol_verb ;
    propol_objects = List.map (expand_object env) t.propol_objects ;
  }

and expand_triples_block env t =
  { triples_loc = t.triples_loc ;
    triples = List.map (expand_triples_same_subject env) t.triples ;
  }

and expand_triples_same_subject env = function
  | TriplesVar (t, l) ->
      TriplesVar
        (expand_var_or_term env t,
         List.map (expand_prop_object_list env) l
        )
  | TriplesNode (t, l) ->
      TriplesNode
        (expand_triples_node env t,
         List.map (expand_prop_object_list env) l
        )

and expand_ggp_sub env t =
  {
    ggp_sub_loc = t.ggp_sub_loc ;
    ggp_sub_elts = List.map (expand_graph_pattern_elt env) t.ggp_sub_elts ;
  }

and expand_group_graph_pattern env = function
  | SubSelect s -> SubSelect (expand_sub_select env s)
  | GGPSub g -> GGPSub (expand_ggp_sub env g)

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


let expand_triples_template env l = List.map (expand_triples_same_subject env) l
let expand_construct_template = expand_triples_template

let expand_construct_where env = function
  | Constr_ggp p -> Constr_ggp (expand_group_graph_pattern env p)
  | Constr_template t -> Constr_template (expand_triples_template env t)

let expand_construct_query env t =
  {
    constr_template = map_opt (expand_construct_template env) t.constr_template ;
    constr_dataset = List.map (expand_dataset_clause env) t.constr_dataset ;
    constr_where = expand_construct_where env t.constr_where ;
    constr_modifier = expand_solution_modifier env t.constr_modifier ;
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

let build_dataset =
  let iter ds = function
  | DefaultGraphClause (PrefixedName _)
  | NamedGraphClause (PrefixedName _) -> assert false
  | DefaultGraphClause (Reliri _)
  | NamedGraphClause (Reliri _) -> assert false
  | DefaultGraphClause (Iriref ir) ->
      { ds with from = Some ir.ir_iri }
  | NamedGraphClause (Iriref ir) ->
      { ds with
        from_named = Rdf_iri.Iriset.add ir.ir_iri ds.from_named }
  in
  let build clauses = List.fold_left iter
    { from = None ; from_named = Rdf_iri.Iriset.empty } clauses
  in
  function
    Select q -> build q.select_dataset
  | Construct q -> build q.constr_dataset
  | Describe q -> build q.desc_dataset
  | Ask q -> build q.ask_dataset
;;

let expand_query default_base_uri q =
  let env = create_env default_base_uri in
  let (env, q_prolog) = expand_query_prolog env q.q_prolog in
  let q_kind = expand_query_kind env q.q_kind in
  let q_values = expand_values_clause env q.q_values in
  let ds = build_dataset q.q_kind in
  (env.base, ds, { q_prolog ; q_kind ; q_values })
;;
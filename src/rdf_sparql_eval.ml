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

module N = Rdf_node
open Rdf_sparql_types
open Rdf_sparql_algebra

let dbg = Rdf_misc.create_log_fun
  ~prefix: "Rdf_sparql_eval"
    "RDF_SPARQL_EVAL_DEBUG_LEVEL"
;;

module Irimap = Map.Make
  (struct type t = Rdf_uri.uri let compare = Rdf_uri.compare end)

type context =
  { graphs : Rdf_graph.graph Irimap.t ;
    active : Rdf_graph.graph ;
  }

module GExprOrdered =
  struct
    type t = Rdf_node.node option list
    let compare =
      let comp a b =
        match a, b with
          None, None -> 0
        | Some _, None -> 1
        | None, Some _ -> -1
        | Some a, Some b -> Rdf_node.Ord_type.compare a b
      in
      Rdf_misc.compare_list comp
  end
module GExprMap = Map.Make (GExprOrdered)



let eval_expr : context -> Rdf_sparql_ms.mu -> expression -> Rdf_node.literal =
  fun ctx mu e -> assert false

(** Evaluate boolean expression.
  See http://www.w3.org/TR/sparql11-query/#ebv *)
let ebv ctx mu e =
  let lit =  eval_expr ctx mu e in
  match lit.N.lit_type with
  | Some t when Rdf_uri.equal t Rdf_rdf.xsd_boolean ->
      lit.N.lit_value = "true"
  | Some t when Rdf_uri.equal t Rdf_rdf.xsd_integer ->
      begin
        try (int_of_string lit.N.lit_value) <> 0
        with _ -> false
      end
  | Some t when Rdf_uri.equal t Rdf_rdf.xsd_double
        or Rdf_uri.equal t Rdf_rdf.xsd_decimal ->
      begin
        try
          let v = float_of_string lit.N.lit_value in
          match Pervasives.classify_float v with
            FP_nan -> false
          | _ -> v <> 0.0
        with _ -> false
      end
  | Some t when Rdf_uri.equal t Rdf_rdf.xsd_string ->
      String.length lit.N.lit_value > 0
  | _ ->
      String.length lit.N.lit_value > 0

let eval_filter ctx mu c =
  let e =
    match c with
      ConstrBuiltInCall c ->
        { expr_loc = Rdf_sparql_types.dummy_loc ; expr = EBic c }
    | ConstrFunctionCall c ->
        { expr_loc = Rdf_sparql_types.dummy_loc ; expr = EFuncall c }
    | ConstrExpr e -> e
  in
  ebv ctx mu e


let filter_omega =
  let pred ctx filters mu = List.for_all (eval_filter ctx mu) filters in
  fun ctx filters o -> Rdf_sparql_ms.omega_filter (pred ctx filters) o

let join_omega ctx o1 o2 =
  Rdf_sparql_ms.omega_join o1 o2

let union_omega o1 o2 = Rdf_sparql_ms.omega_union o1 o2

let leftjoin_omega =
  let pred ctx filters mu = List.for_all (eval_filter ctx mu) filters in
  fun ctx o1 o2 filters ->
    let pred = pred ctx filters in
    let filter_part = Rdf_sparql_ms.omega_join ~pred o1 o2 in
    let diff_part = Rdf_sparql_ms.omega_diff_pred pred o1 o2 in
    union_omega filter_part diff_part

let minus_omega o1 o2 = Rdf_sparql_ms.omega_minus o1 o2

let extend_omega ctx o var expr =
  let eval mu = Rdf_node.Literal (eval_expr ctx mu expr) in
  Rdf_sparql_ms.omega_extend eval o var

let sort_sequence ctx l = l

let project_sequence vars l =
  let vars = Rdf_sparql_algebra.VS.fold
    (fun v acc -> Rdf_sparql_types.SSet.add v.var_name acc)
      vars Rdf_sparql_types.SSet.empty
  in
  List.map (Rdf_sparql_ms.mu_project vars) l

let distinct =
  let f (set, acc) mu =
    if Rdf_sparql_ms.MuSet.mem mu set then
      (set, acc)
    else
      (Rdf_sparql_ms.MuSet.add mu set, mu :: acc)
  in
  fun l ->
    let (_, l) = List.fold_left f (Rdf_sparql_ms.MuSet.empty, []) l in
    List.rev l
;;

let slice =
  let rec until len acc i = function
    [] -> List.rev acc
  | _ when i >= len -> List.rev acc
  | h :: q -> until len (h::acc) (i+1) q
  in
  let rec iter start len i = function
    [] -> []
  | h :: q when i < start -> iter start len (i+1) q
  | q ->
      match len with
        None -> q
      | Some len -> until len [] 0 q
  in
  fun l off lim ->
    match off, lim with
      None, None -> l
    | Some off, None -> iter off None 0 l
    | None, Some lim -> until lim [] 0 l
    | Some off, Some lim -> iter off (Some lim) 0 l
;;

let group_omega =
  let make_e expr = { expr_loc = Rdf_sparql_types.dummy_loc ; expr } in
  let map_conds = function
  | GroupBuiltInCall c -> make_e (EBic c)
  | GroupFunctionCall c -> make_e (EFuncall c)
  | GroupVar gv ->
      match gv.grpvar_expr, gv.grpvar with
        None, None -> assert false
      | Some e, None -> e
      | None, Some v -> make_e (EVar v)
      | Some e, Some v -> assert false (* what to evaluate ? *)
  in
  let eval_one ctx mu e =
    try Some(Rdf_node.Literal (eval_expr ctx mu e))
    with _ -> None
  in

  fun ctx conds o ->
    let conds = List.map map_conds conds in
    let eval ctx mu = List.map (eval_one ctx mu) conds in
    Rdf_sparql_ms.omega_fold_n
      (fun mu n acc ->
         let v = eval ctx mu in
         let o =
           try GExprMap.find v acc
           with Not_found -> Rdf_sparql_ms.MuMap.empty
         in
         let o = Rdf_sparql_ms.omega_add mu o in
         GExprMap.add v o acc
      )
      o
      GExprMap.empty

let agg_count d ms eopt = assert false
let agg_sum d ms e = assert false
let agg_min d ms e = assert false
let agg_max d ms e = assert false
let agg_avg d ms e = assert false
let agg_sample d ms e = assert false
let agg_group_concat d ms e sopt = assert false

let eval_agg ctx agg ms =
  match agg with
    Bic_COUNT (d, eopt) -> agg_count d ms eopt
  | Bic_SUM (d, e) -> agg_sum d ms e
  | Bic_MIN (d, e) -> agg_min d ms e
  | Bic_MAX (d, e) -> agg_max d ms e
  | Bic_AVG (d, e) -> agg_avg d ms e
  | Bic_SAMPLE (d, e) ->
      let (sample_mu,_) =
        try Rdf_sparql_ms.MuMap.choose ms
        with Not_found -> assert false
      in
      eval_expr ctx sample_mu e
  | Bic_GROUP_CONCAT (d, e, s_opt) -> agg_group_concat d ms e s_opt
;;
let aggregation ctx agg groups =
  let f ms = eval_agg ctx agg ms in
  GExprMap.map f groups
;;

let aggregate_join =
  let compute_agg ctx ms (i,acc_mu) = function
    Aggregation agg ->
      let term = Rdf_node.Literal (eval_agg ctx agg ms) in
      let var = "__agg"^(string_of_int (i+1)) in
      (i+1, Rdf_sparql_ms.mu_add var term acc_mu)
  | _ -> assert false
  in
  let compute_group ctx aggs key ms acc =
    let (_,mu) = List.fold_left (compute_agg ctx ms) (1,Rdf_sparql_ms.mu_0) aggs in
    Rdf_sparql_ms.omega_add mu acc
  in
  fun eval ctx (conds, a) aggs ->
    let o = eval ctx a in
    let groups = group_omega ctx conds o in
    GExprMap.fold (compute_group ctx aggs) groups Rdf_sparql_ms.MuMap.empty

let cons h q = h :: q ;;

let filter_of_var_or_term = function
  Rdf_sparql_types.Var v -> (Some v.var_name, None)
| GraphTerm t ->
    match t with
      GraphTermIri (Iriref ir) -> (None, Some (Rdf_node.Uri ir.ir_iri))
    | GraphTermIri (PrefixedName _) -> assert false
    | GraphTermLit lit
    | GraphTermNumeric lit
    | GraphTermBoolean lit -> (None, Some (Rdf_node.Literal lit.rdf_lit))
    | GraphTermBlank bn ->
         let s =
           match bn.bnode_label with
             None -> None
           | Some s -> Some ("?"^s)
         in
         (s, None)
    | GraphTermNil -> (None, None)

let eval_simple_triple =
  let add mu term = function
    None -> mu
  | Some name -> Rdf_sparql_ms.mu_add name term mu
  in
  fun ctx x path y ->
    dbg ~level: 2
      (fun () ->
         "eval_simple_triple "^
         (Rdf_sparql_algebra.string_of_triple (x, path, y))
      );
    let (vx, sub) = filter_of_var_or_term x in
    let (vy, obj) = filter_of_var_or_term y in
    let (vp, pred) =
      match path with
        Var v -> (Some v.var_name, None)
      | Iri ir -> (None, Some (Rdf_node.Uri ir.ir_iri))
      | _ -> assert false
    in
    let f acc (s,p,o) =
      dbg ~level: 3
        (fun () ->
           "simple_triple__f("^
             (Rdf_node.string_of_node s)^", "^
             (Rdf_node.string_of_node p)^", "^
             (Rdf_node.string_of_node o)^")"
        );
      let mu = add Rdf_sparql_ms.mu_0 s vx in
      let mu = add mu p vp in
      let mu = add mu o vy in
      Rdf_sparql_ms.omega_add mu acc
    in
    (* FIXME: we will use a fold in the graph when it is implemented *)
    List.fold_left f Rdf_sparql_ms.MuMap.empty
      (ctx.active.Rdf_graph.find ?sub ?pred ?obj ())

let __print_mu mu =
  Rdf_sparql_ms.SMap.iter
    (fun name term -> print_string (name^"->"^(Rdf_node.string_of_node term)^" ; "))
    mu;
  print_newline ()
;;

let __print_omega o =
  Rdf_sparql_ms.omega_iter __print_mu o;;

let rec eval_triples =
  let eval_join ctx acc triple =
    let o = eval_triple ctx triple in
    Rdf_sparql_ms.omega_join acc o
  in
  fun ctx -> function
        [] -> Rdf_sparql_ms.omega_0
    | l -> List.fold_left (eval_join ctx) Rdf_sparql_ms.omega_0 l

and eval_triple ctx (x, path, y) =
  match path with
    Var _
  | Iri _ -> eval_simple_triple ctx x path y
  | Inv p -> eval_triple ctx (y, p, x)
  | _ -> failwith "not implemented"

and eval ctx = function
| BGP triples ->
      let om = eval_triples ctx triples in
      __print_omega om;
      om

| Join (a1, a2) ->
    let o1 = eval ctx a1 in
    let o2 = eval ctx a2 in
    join_omega ctx o1 o2

| LeftJoin (a1, a2, filters) ->
    let o1 = eval ctx a1 in
    let o2 = eval ctx a2 in
    leftjoin_omega ctx o1 o2 filters

| Filter (a, filters) ->
      let omega = eval ctx a in
      filter_omega ctx filters omega

| Union (a1, a2) ->
    let o1 = eval ctx a1 in
    let o2 = eval ctx a2 in
    union_omega o1 o2

| Graph (var_or_iri, a) -> assert false

| Extend (a, var, expr) ->
    let o = eval ctx a in
    extend_omega ctx o var expr

| Minus (a1, a2) ->
    let o1 = eval ctx a1 in
    let o2 = eval ctx a2 in
    minus_omega o1 o2

| ToMultiset a ->
    let l = eval_list ctx a in
    List.fold_left
      (fun o mu -> Rdf_sparql_ms.omega_add mu o)
      Rdf_sparql_ms.MuMap.empty l

| AggregateJoin (Group(conds,a), l) ->
    aggregate_join eval ctx (conds,a) l

| AggregateJoin _ -> assert false (* AggregationJoin always has a Group *)
| Aggregation _ -> assert false (* Aggregation always below AggregateJoin *)
| Group (conds, a) -> assert false (* no group without AggregationJoin above *)

| DataToMultiset datablock -> assert false
| Project _ -> assert false
| Distinct a -> assert false
| Reduced a -> assert false
| Slice (a, offset, limit) -> assert false
| OrderBy (a, order_conds) -> assert false

and eval_list ctx = function
  | OrderBy (a, order_conds) ->
      let l = eval_list ctx a in
      sort_sequence ctx l
  | Project (a, vars) ->
      let l = eval_list ctx a in
      project_sequence vars l
  | Distinct a ->
      let l = eval_list ctx a in
      distinct l
  | Reduced a ->
      let l = eval_list ctx a in
      distinct l (* FIXME: still have to understand what Reduced means *)
  | Slice (a, off, lim) ->
      let l = eval_list ctx a in
      slice l off lim
  | a ->
      let o = eval ctx a in
      Rdf_sparql_ms.omega_fold cons o []





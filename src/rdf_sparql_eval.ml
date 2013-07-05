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
open Rdf_sparql_algebra

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


(** Evaluate boolean expression *)
let ebv ctx mu e = true

let eval_expr : context -> Rdf_sparql_ms.mu -> expression -> Rdf_node.node =
  fun ctx mu e -> assert false

let filter_omega =
  let pred ctx filters mu = List.for_all (ebv ctx mu) filters in
  fun ctx filters o -> Rdf_sparql_ms.omega_filter (pred ctx filters) o

let join_omega ctx o1 o2 =
  Rdf_sparql_ms.omega_join o1 o2

let union_omega o1 o2 = Rdf_sparql_ms.omega_union o1 o2

let leftjoin_omega =
  let pred ctx filters mu = List.for_all (ebv ctx mu) filters in
  fun ctx o1 o2 filters ->
    let pred = pred ctx filters in
    let filter_part = Rdf_sparql_ms.omega_join ~pred o1 o2 in
    let diff_part = Rdf_sparql_ms.omega_diff_pred pred o1 o2 in
    union_omega filter_part diff_part

let minus_omega o1 o2 = Rdf_sparql_ms.omega_minus o1 o2

let extend_omega ctx o var expr =
  let eval mu = eval_expr ctx mu expr in
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
    try Some(eval_expr ctx mu e)
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
  let f eval ctx = function
    Aggregation (agg, Group (conds, a)) ->
      let o = eval ctx a in
      let groups = group_omega ctx conds o in
      aggregation ctx agg groups
  | _ -> assert false
  in
  let gather i v =
    let var = {
        var_loc = Rdf_sparql_types.dummy_loc ;
        var_name = "__agg"^(string_of_int (i+1)) ;
      }
    in
    (var, v)
  in
  let make_mu l =
    List.fold_left
      (fun acc (var, v) -> Rdf_sparql_ms.mu_add var v acc)
      Rdf_sparql_ms.SMap.empty
      (List.mapi gather l)
  in
  fun eval ctx l ->
    let l = List.map (f eval ctx) l in
    make_mu l




let cons h q = h :: q ;;

let rec eval ctx = function
| BGP triples -> assert false

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

| AggregateJoin l ->
    aggregate_join eval ctx l

| Aggregation _ -> assert false (* Aggregation always below AggregateJoin *)
| Group (conds, a) -> assert false (* no group without Aggregation above *)
| Aggregation (_, _) -> assert false (* Aggregation always contains a Group *)

| DataToMultiset datablock -> assert false
| AggregateJoin l -> assert false
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





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
open Rdf_dt
open Rdf_sparql_types
open Rdf_sparql_algebra

let dbg = Rdf_misc.create_log_fun
  ~prefix: "Rdf_sparql_eval"
    "RDF_SPARQL_EVAL_DEBUG_LEVEL"
;;

exception Unbound_variable of var
exception Not_a_integer of Rdf_node.literal
exception Not_a_double_or_decimal of Rdf_node.literal
exception Type_mismatch of Rdf_dt.value * Rdf_dt.value
exception Invalid_fun_argument of Rdf_uri.uri
exception Unknown_fun of Rdf_uri.uri
exception Invalid_built_in_fun_argument of string * expression list
exception Unknown_built_in_fun of string
exception No_term
exception Cannot_compare_for_datatype of Rdf_uri.uri

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

(** Evaluate boolean expression.
  See http://www.w3.org/TR/sparql11-query/#ebv *)
let ebv = function
  | Error e -> raise e
  | Bool b -> b
  | String "" -> false
  | String _ -> true
  | Ltrl ("",_) -> false
  | Ltrl _ -> true
  | Ltrdt ("", _) -> false
  | Ltrdt _ -> true
  | Int n -> n <> 0
  | Float f ->
      begin
        match Pervasives.classify_float f with
          FP_nan | FP_zero -> false
        | _ -> true
      end
  | Datetime _
  | Rdf_dt.Iri _ | Rdf_dt.Blank _ -> false (* FIXME: or error ? *)
;;


let rec compare ?(sameterm=false) v1 v2 =
  match v1, v2 with
  | Error _, _ -> 1
  | _, Error _ -> -1
  | Rdf_dt.Iri t1, Rdf_dt.Iri t2 -> Rdf_uri.compare t1 t2
  | Rdf_dt.Blank s1, Rdf_dt.Blank s2 -> Pervasives.compare s1 s2
  | String s1, String s2
  | Ltrl (s1, None), String s2
  | String s1, Ltrl (s2, None) -> Pervasives.compare s1 s2
  | Int n1, Int n2 -> Pervasives.compare n1 n2
  | Int _, Float _ -> compare (Rdf_dt.float v1) v2
  | Float _, Int _ -> compare v1 (Rdf_dt.float v2)
  | Float f1, Float f2 -> Pervasives.compare f1 f2
  | Bool b1, Bool b2 -> Pervasives.compare b1 b2
  | Datetime t1, Datetime t2 ->
      Pervasives.compare (Netdate.since_epoch t1) (Netdate.since_epoch t2)
  | Ltrl (l1, lang1), Ltrl (l2, lang2) ->
      begin
        match Pervasives.compare lang1 lang2 with
          0 -> Pervasives.compare l1 l2
        | n -> n
      end
  | Ltrdt (s1, dt1), Ltrdt (s2, dt2) ->
      (
       match Rdf_uri.compare dt1 dt2 with
         0 ->
           if sameterm then
             Pervasives.compare s1 s2
           else
             raise (Cannot_compare_for_datatype dt1)
       | _ -> raise (Type_mismatch (v1, v2))
      )
  | _, _ -> raise (Type_mismatch (v1, v2))

(**  Predefined functions *)

let xsd_datetime = Rdf_rdf.xsd_ "dateTime";;
let fun_datetime = function
  [] | _::_::_ -> raise(Invalid_fun_argument xsd_datetime)
| [v] -> Rdf_dt.datetime v

let funs = [
    xsd_datetime, fun_datetime ;
  ];;

let funs = List.fold_left
  (fun acc (iri, f) -> Irimap.add iri f acc) Irimap.empty funs;;


(** Builtin functions; they take an expression evaluation function
  in parameter, as all arguments must not be always evaluated,
  for example in the IF.  *)

let bi_if name eval_expr = function
  [e1 ; e2 ; e3] ->
    begin
       if ebv (eval_expr e1) then
         eval_expr e2
       else
         eval_expr e3
    end
| l ->
  raise (Invalid_built_in_fun_argument (name, l))
;;

let bi_coalesce _ =
  let rec iter eval_expr = function
    [] -> raise No_term
  | h :: q ->
    let v =
        try
          match eval_expr h with
            Error _ -> None
          | v -> Some v
        with _ -> None
      in
      match v with
        None -> iter eval_expr q
      | Some v -> v
  in
  fun eval_expr l ->
    iter eval_expr l
;;

let bi_sameterm name =
  let f eval_expr = function
    [e1 ; e2] ->
      let v1 = eval_expr e1 in
      let v2 = eval_expr e2 in
      Bool (compare ~sameterm: true v1 v2 = 0)
  | l -> raise (Invalid_built_in_fun_argument (name, l))
  in
  f
;;

let built_in_funs =
  let l =
    [ "IF", bi_if ;
      "COALESCE", bi_coalesce ;
      "SAMETERM", bi_sameterm ;
    ]
  in
  List.fold_left
    (fun acc (name, f) -> SMap.add name (f name) acc)
    SMap.empty l
;;



let get_built_in_fun name =
  let name = String.uppercase name in
  try SMap.find name built_in_funs
  with Not_found -> raise (Unknown_built_in_fun name)
;;

let eval_var mu v =
  try
    let node = Rdf_sparql_ms.mu_find_var v mu in
    Rdf_dt.of_node node
  with Not_found -> raise (Unbound_variable v)
;;


let rec eval_numeric2 f_int f_float (v1, v2) =
 try
   match (v1, v2) with
    | (Float f1, Float f2) -> Float (f_float f1 f2)
    | (Int n1, Int n2) -> Int (f_int n1 n2)
    | ((Float _) as v1, ((Int _) as v2)) ->
        eval_numeric2 f_int f_float (v1, Rdf_dt.float v2)
    | ((Int _) as v1, ((Float _) as v2)) ->
        eval_numeric2 f_int f_float (Rdf_dt.float v1, v2)
    | v1, v2 ->
        eval_numeric2 f_int f_float
          ((Rdf_dt.numeric v1), (Rdf_dt.numeric v2))
  with
    e -> Error e
;;

let eval_plus = eval_numeric2 (+) (+.)
let eval_minus = eval_numeric2 (-) (-.)
let eval_mult = eval_numeric2 ( * ) ( *. )
let eval_div = eval_numeric2 (/) (/.)

let eval_equal (v1, v2) = Bool (compare v1 v2 = 0)
let eval_not_equal (v1, v2) = Bool (compare v1 v2 <> 0)
let eval_lt (v1, v2) = Bool (compare v1 v2 < 0)
let eval_lte (v1, v2) = Bool (compare v1 v2 <= 0)
let eval_gt (v1, v2) = Bool (compare v1 v2 > 0)
let eval_gte (v1, v2) = Bool (compare v1 v2 >= 0)

let eval_or = function
  (Error e, Error _) -> Error e
| (Error e, v)
| (v, Error e) ->
    if ebv v then Bool true else Error e
| v1, v2 -> Bool ((ebv v1) || (ebv v2))

let eval_and = function
  (Error e, Error _) -> Error e
| (Error e, v)
| (v, Error e) ->
    if ebv v then Error e else Bool false
| v1, v2 -> Bool ((ebv v1) && (ebv v2))

let eval_bin = function
| EPlus -> eval_plus
| EMinus -> eval_minus
| EMult -> eval_mult
| EDiv -> eval_div
| EEqual -> eval_equal
| ENotEqual -> eval_not_equal
| ELt -> eval_lt
| EGt -> eval_gt
| ELte -> eval_lte
| EGte -> eval_gte
| EOr -> eval_or
| EAnd -> eval_and

let rec eval_expr : context -> Rdf_sparql_ms.mu -> expression -> Rdf_dt.value =
  fun ctx mu e ->
    match e.expr with
      EVar v -> eval_var mu v
    | EBin (e1, op, e2) ->
        let v1 = eval_expr ctx mu e1 in
        let v2 = eval_expr ctx mu e2 in
        eval_bin op (v1, v2)
    | ENot e ->
        let b = ebv (eval_expr ctx mu e) in
        Bool (not b)
    | EUMinus e ->
        let v = eval_expr ctx mu e in
        eval_bin EMinus (Int 0, v)
    | EBic c -> eval_bic ctx mu c
    | EFuncall c -> eval_funcall ctx mu c
    | ELit lit
    | ENumeric lit
    | EBoolean lit -> Rdf_dt.of_literal lit.rdf_lit
    | EIn (e, l) -> eval_in ctx mu e l
    | ENotIn (e, l) ->
        match eval_in ctx mu e l with
          Bool b -> Bool (not b)
        | Error e -> Error e
        | _ -> assert false

and eval_bic ctx mu = function
  | Bic_agg agg -> assert false
  | Bic_fun (name, args) ->
      let f = get_built_in_fun name in
      f (eval_expr ctx mu) args
  | Bic_BOUND v ->
      (try ignore(Rdf_sparql_ms.mu_find_var v mu); Bool true
       with _ -> Bool false)
  | Bic_EXISTS _
  | Bic_NOTEXISTS _ -> assert false
     (* FIXME: need to translate this in algebra, with type parameter for expressions ... ? *)

and eval_funcall ctx mu c =
  let f =
    let iri =
      match c.func_iri with
        Iriref ir -> ir.ir_iri
      | _ -> assert false
    in
    try Irimap.find iri funs
    with Not_found -> raise (Unknown_fun iri)
  in
  let args = List.map (eval_expr ctx mu) c.func_args.argl in
  f args

and eval_in =
  let eval eval_expr ctx mu v0 e acc =
    let v = eval_expr ctx mu e in
    let b =
      try Bool (compare v0 v = 0)
      with e -> Error e
    in
    eval_or (b, acc)
  in
  fun ctx mu e0 l ->
    match l with
      [] -> Bool false
    | _ ->
      let v0 = eval_expr ctx mu e0 in
      List.fold_right (eval eval_expr ctx mu v0) l (Bool false)

and ebv_lit v = Rdf_node.mk_literal_bool (ebv v)

let eval_filter ctx mu c =
  let e =
    match c with
      ConstrBuiltInCall c ->
        { expr_loc = Rdf_sparql_types.dummy_loc ; expr = EBic c }
    | ConstrFunctionCall c ->
        { expr_loc = Rdf_sparql_types.dummy_loc ; expr = EFuncall c }
    | ConstrExpr e -> e
  in
  ebv (eval_expr ctx mu e)


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
  let eval mu = Rdf_dt.to_node (eval_expr ctx mu expr) in
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
    try Some(Rdf_dt.to_node (eval_expr ctx mu e))
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
      let term = Rdf_dt.to_node (eval_agg ctx agg ms) in
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





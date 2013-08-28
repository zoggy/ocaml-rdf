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

type error =
| Variable_already_defined of var
| Unknown_prefix of pname_ns

exception Error of error
let error e = raise (Error e)

let string_of_error = function
| Variable_already_defined v ->
    Printf.sprintf "%sMultiply defined variable %S"
      (Rdf_loc.string_of_loc v.var_loc) v.var_name
| Unknown_prefix p ->
   Printf.sprintf "%sUnknown prefix %S"
      (Rdf_loc.string_of_loc p.pname_ns_loc)
      p.pname_ns_name
;;


type query =
    {
      query_proj : select_clause option ;
      query_where : group_graph_pattern ;
      query_modifier : solution_modifier ;
      query_values : values_clause ;
    }

type filter = constraint_


exception Implicit_aggregate_found

type path =
    | Var of var
    | Iri of iriref
    | Inv of path
    | Alt of path * path
    | Seq of path * path
    | ZeroOrMore of path
    | OneOrMore of path
    | ZeroOrOne of path
    | NPS of iriref list

type triple = var_or_term * path * var_or_term

module VS = Rdf_sparql_types.VarSet

type algebra =
  | BGP of triple list
  | Join of algebra * algebra
  | LeftJoin of algebra * algebra * filter list
  | Filter of algebra * filter list
  | Union of algebra * algebra
  | Graph of var_or_iri * algebra
  | Extend of algebra * var * expression
  | Minus of algebra * algebra
  | ToMultiset of algebra
  | DataToMultiset of datablock
  | Group of group_condition list * algebra
  | Aggregation of aggregate
  | AggregateJoin of algebra * algebra list (* Group * Aggregation list *)
  | Project of algebra * VS.t
  | Distinct of algebra
  | Reduced of algebra
  | Slice of algebra * int option * int option (* algebra * offset * limit *)
  | OrderBy of algebra * order_condition list

let visible_vars =
  (* list of all variables visible in the pattern,
     so restricted by sub-SELECT projected variables and GROUP BY variables.
     Not visible: only in filter, exists/not exists, masked by a subselect,
       non-projected GROUP variables, only in the right hand side of MINUS
  *)
  let f_var f acc t = VS.add t acc in
  let f_bind f acc t = VS.add t.bind_var acc in
  let f_group_var f acc t =
    match t.grpvar with
      None -> acc
    | Some v -> VS.add v acc
  in
  let f_sel_var acc sv = VS.add sv.sel_var acc in
  let f_sub_select f acc t =
    (* keep only named variables in select and group by clauses *)
    let acc =
      match t.subsel_select.sel_vars with
        SelectAll -> acc
      | SelectVars l -> List.fold_left f_sel_var acc l
    in
    let f acc = function
      GroupVar { grpvar = Some v} -> VS.add v acc
    | _ -> acc
    in
    List.fold_left f acc (t.subsel_modifier.solmod_group)
  in
  let visitor =
    { Rdf_sparql_vis.default with
      Rdf_sparql_vis.var = f_var ;
      bind = f_bind ;
      group_var = f_group_var ;
      sub_select = f_sub_select ;
    }
  in
  fun q ->
    visitor.Rdf_sparql_vis.group_graph_pattern visitor VS.empty q.query_where

let collect_and_remove_filters l =
  let f (acc_constraints, acc) = function
  | Rdf_sparql_types.Filter const -> (const :: acc_constraints, acc)
  | x -> (acc_constraints, x :: acc)
  in
  let (constraints, l) = List.fold_left f ([], []) l in
  (List.rev constraints, List.rev l)
;;

let fresh_var =
  let cpt = ref 0 in
  fun () ->
    incr cpt;
    let label = "_V"^(string_of_int !cpt) in
    { var_loc = Rdf_loc.dummy_loc ; var_name = label }
;;

let path_iri_first = Iri { ir_loc = Rdf_loc.dummy_loc ; ir_iri = Rdf_rdf.rdf_first } ;;
let path_iri_rest =  Iri { ir_loc = Rdf_loc.dummy_loc ; ir_iri = Rdf_rdf.rdf_rest } ;;
let iri_nil =  T.Iriref { ir_loc = Rdf_loc.dummy_loc ; ir_iri = Rdf_rdf.rdf_nil } ;;
let iriref_type = { ir_loc = Rdf_loc.dummy_loc ; ir_iri = Rdf_rdf.rdf_type } ;;
let path_iri_type = Iri iriref_type ;;

let rec translate_path = function
| [] -> assert false
| h :: q ->
    List.fold_left
      (fun acc ps -> Alt (acc, translate_path_sequence ps))
      (translate_path_sequence h)
      q

and translate_path_sequence = function
  | [] -> assert false
  | h :: q ->
      List.fold_left
        (fun acc ps -> Seq (acc, translate_path_elt_or_inverse ps))
        (translate_path_elt_or_inverse h)
        q

and translate_path_elt_or_inverse = function
  Elt e -> translate_path_elt e
| T.Inv e -> Inv(translate_path_elt e)

and translate_path_elt e =
  let p = translate_path_primary e.pelt_primary in
  match e.pelt_mod with
    None -> p
  | Some m ->
      match m with
      | ModOptional -> ZeroOrOne p
      | ModList -> ZeroOrMore p
      | ModOneOrMore -> OneOrMore p

and translate_path_primary = function
  | PathIri (PrefixedName _) -> assert false
  | PathIri (Iriref ir) -> Iri ir
  | PathA -> path_iri_type
  | Path p -> translate_path p
  | PathNegPropSet l ->
      let pos, inv = partition_path_one_in_prop_set l in
      match pos, inv with
        _, [] -> NPS pos
      | [], _ -> Inv (NPS inv)
      | _, _ -> Alt (NPS pos, Inv (NPS inv))

and partition_path_one_in_prop_set =
  let rec iter (acc, acc_inv) = function
    [] -> (acc, acc_inv)
  | h :: q ->
     match h with
     | PathOneInIri (PrefixedName _) -> assert false
     | PathOneInIri (Iriref iriref) -> (iriref :: acc, acc_inv)
     | PathOneInA -> (iriref_type :: acc, acc_inv)
     | PathOneInNotIri (PrefixedName _) -> assert false
     | PathOneInNotIri (Iriref iriref)-> (acc, iriref :: acc_inv)
     | PathOneInNotA -> (acc, iriref_type :: acc_inv)
  in
  iter ([], [])

let rec translate_property_path_pattern acc = function
| (x, Inv path, y) -> translate_property_path_pattern acc (y, path, x)
| (x, Seq (p1, p2), y) ->
    let v = T.Var (fresh_var()) in
    let acc = translate_property_path_pattern acc (x, p1, v) in
    translate_property_path_pattern acc (v, p2, y)
| (x, p, y) -> (x, p, y) :: acc

let translate_property_path_patterns =
  List.fold_left translate_property_path_pattern []
;;

let rec build_triples_path subject acc prop_obj_list =
  let path =
    match prop_obj_list.propol_verb with
    | VerbPath path -> translate_path path
    | VerbVar var -> Var var
    | VerbIri (PrefixedName _) -> assert false
    | VerbIri (T.Iriref iriref) -> Iri iriref
    | VerbA -> path_iri_type
  in
  List.fold_left
    (build_triples_prop_graph_node subject path)
    acc prop_obj_list.propol_objects

and build_triples_prop_graph_node subject prop acc = function
  | GraphNodeVT v_or_t -> (subject, prop, v_or_t) :: acc
  | GraphNodeTriples triples_node_path ->
      let v = T.Var (fresh_var ()) in
      let acc = (subject, prop, v) :: acc in
      match triples_node_path with
      | TNodeCollection l ->
          build_triples_path_collection acc v l
      | TNodeBlank l ->
          List.fold_left (build_triples_path v) acc l

and build_triples_path_collection acc subject = function
    | [] -> acc
    | h :: q ->
        let acc = build_triples_prop_graph_node subject path_iri_first acc h in
        match q with
          [] ->
            (subject, path_iri_rest, T.GraphTerm (T.GraphTermIri iri_nil)) :: acc
        | _ ->
            let v = T.Var (fresh_var()) in
            let acc = (subject, path_iri_rest, v) :: acc in
            build_triples_path_collection acc v q


and translate_triples_same_subject_path acc t =
    let (acc, subject, prop_obj_list) =
       match t with
         T.TriplesVar (t, l) ->
           (acc, t, l)
       | T.TriplesNode (triples_node, l) ->
          let v = T.Var (fresh_var ()) in
          match triples_node with
          | TNodeCollection gnl ->
             let acc = build_triples_path_collection acc v gnl in
             (acc, v, l)
          | TNodeBlank pol ->
             let acc = List.fold_left (build_triples_path v) acc pol in
             (acc, v, l)
    in
    let triples = List.fold_left (build_triples_path subject) acc prop_obj_list in
    translate_property_path_patterns triples

(** Translate group graph pattern as explained here:
  http://www.w3.org/TR/sparql11-query#convertGraphPattern *)
let rec translate_ggp = function
  SubSelect t -> ToMultiset (translate_subselect t)
| GGPSub t -> translate_ggp_sub t

and translate_subselect t =
    let q =
      { query_proj = Some t.subsel_select ;
        query_where = t.subsel_where ;
        query_modifier = t.subsel_modifier ;
        query_values = t.subsel_values ;
      }
    in
    translate_query_level q

and translate_ggp_sub t =
    let (filters, l) = collect_and_remove_filters t.ggp_sub_elts in
    let f g elt =
      match elt with
      | T.Triples l -> Join(g, translate_triples_block l)
      | T.Union l -> Join(g, translate_union l)
      | T.Optional g2 ->
          (
           match translate_ggp g2 with
             Filter (g2, f) -> LeftJoin(g, g2, f)
           | g2 -> LeftJoin(g, g2, [])
          )
      | T.Minus g2 -> Minus(g, translate_ggp g2)
      | T.GGP t -> Join(g, Graph(t.graphgp_name, translate_ggp t.graphgp_pat))
      | T.Bind bind -> Extend (g, bind.bind_var, bind.bind_expr)
      | T.Service s -> Join (g, translate_service s)
      | T.InlineData d -> Join (g, translate_inline_data d)
      | T.Filter c -> assert false
    in
    let g = List.fold_left f (BGP []) l in
    match filters with
      [] -> g
    | _ -> Filter (g, filters)

and translate_triples_block t =
  BGP (List.fold_left translate_triples_same_subject_path [] t.triples)

and translate_union l =
  match l with
    [] -> BGP []
  | [g] -> translate_ggp g
  | h :: q ->
      List.fold_left
        (fun acc g -> Union(acc, translate_ggp g))
        (translate_ggp h) q

and translate_service s = failwith "SPARQL algebra: translate_service not implemented"

and translate_inline_data d = DataToMultiset d

and has_implicit_grouping q =
  let aggregate f acc _ = raise Implicit_aggregate_found in
  let visitor = { Rdf_sparql_vis.default with Rdf_sparql_vis.aggregate = aggregate } in
  try
    ignore (visitor.Rdf_sparql_vis.group_graph_pattern visitor () q.query_where);
    (match q.query_proj with
         | None -> ()
         | Some c -> ignore (visitor.Rdf_sparql_vis.select_clause visitor () c)
    );
    false
  with Implicit_aggregate_found -> true

and aggregation_step q g =
  let _A = ref [] in
  let agg_i =
     let cpt = ref 0 in
     fun () -> incr cpt;
     { var_loc = Rdf_loc.dummy_loc ;
       var_name = "__agg"^(string_of_int !cpt) ;
     }
  in
  (* do not replace expressions in aggregate *)
  let map_sample =
    let f_aggregate f _ t = t in
    let f_expression f acc t =
      match t.expr with
        EVar v ->
          { expr_loc = Rdf_loc.dummy_loc ;
            expr = EBic (Bic_agg (Bic_SAMPLE (false, t))) ;
          }
      | _ -> Rdf_sparql_map.expression f acc t
    in
    { Rdf_sparql_map.default with
      Rdf_sparql_map.aggregate = f_aggregate ;
      Rdf_sparql_map.expression = f_expression ;
    }
  in
  let map_agg =
    let f_expression f acc t =
      match t.expr with
        EBic (Bic_agg agg) ->
          let a = Aggregation agg in
          _A := a :: !_A ;
          let v = agg_i () in
          { expr_loc = Rdf_loc.dummy_loc ;
            expr = EVar v ;
          }
     | _ -> Rdf_sparql_map.expression f acc t
    in
    { Rdf_sparql_map.default with
      Rdf_sparql_map.expression = f_expression ;
    }
  in
  let q =
    let query_proj =
      match q.query_proj with
      | Some ({ sel_vars = SelectVars l } as s) ->
          let replace e =
            let e = map_sample.Rdf_sparql_map.expression map_sample () e in
            map_agg.Rdf_sparql_map.expression map_agg () e
          in
          let f sv =
            { sv with
              sel_var_expr = Rdf_sparql_map.map_opt replace sv.sel_var_expr ;
            }
          in
          Some { s with sel_vars = SelectVars (List.map f l) }
      | x -> x
    in
    let having =
      let f c =
        let c = map_sample.Rdf_sparql_map.constraint_ map_sample () c in
        map_agg.Rdf_sparql_map.constraint_ map_agg () c
      in
      List.map f q.query_modifier.solmod_having
    in
    let order =
      let f cond =
        let cond = map_sample.Rdf_sparql_map.order_condition map_sample () cond in
        map_agg.Rdf_sparql_map.order_condition map_agg () cond
      in
      Rdf_sparql_map.map_opt (List.map f) q.query_modifier.solmod_order
    in
    let query_modifier =
      { q.query_modifier with
        solmod_having = having ;
        solmod_order = order ;
      }
    in
    { q with query_proj ; query_modifier ; }
  in
  let _E =
    match q.query_proj with
    | Some { sel_vars = SelectVars l } ->
        let f acc sv =
          match sv.sel_var_expr with
          | Some _ -> acc
          | None ->
             let v_agg = agg_i () in
             let e = { expr_loc = Rdf_loc.dummy_loc ; expr = EVar sv.sel_var } in
             let agg = Bic_SAMPLE (false, e) in
             let e_agg = { expr_loc = Rdf_loc.dummy_loc ; expr = EVar v_agg } in
             let a = Aggregation agg in
             _A := a :: !_A;
             (sv.sel_var, e_agg) :: acc
        in
        List.fold_left f [] l
    | _ -> []
  in
  let _E = List.rev _E in
  (AggregateJoin (g, (List.rev !_A)), _E, q)


and translate_query_level q =
  let g = translate_ggp q.query_where in
  let visible_vars = visible_vars q in
  (* aggregate
     http://www.w3.org/TR/sparql11-query/#sparqlGroupAggregate
  *)
  let g =
    match q.query_modifier.solmod_group with
      [] ->
       if has_implicit_grouping q then
         (
          let lit = { rdf_lit_loc = Rdf_loc.dummy_loc ;
                      rdf_lit = Rdf_term.mk_literal_int 1 ;
                      rdf_lit_type = None ;
                    }
          in
          let e = { expr_loc = Rdf_loc.dummy_loc ;
                    expr = ENumeric lit ;
                  }
          in
          let gv = { grpvar_loc = Rdf_loc.dummy_loc ;
                     grpvar_expr = Some e ; grpvar = None ;
                   }
          in
          Group([GroupVar gv], g)
         )
       else
         g
   | group_conds -> Group (group_conds, g)
  in
  let (g, env, q) =
    match g with
      Group (conds, _) ->
       aggregation_step q g
    | _ -> (g, [], q)
  in

  (* having
     http://www.w3.org/TR/sparql11-query/#sparqlHavingClause
  *)
  let g =
    match q.query_modifier.solmod_having with
      [] -> g
    | l -> Filter(g, l)
  in
  (* values
    http://www.w3.org/TR/sparql11-query/#sparqlAlgebraFinalValues
  *)
  let g =
    match q.query_values with
      None -> g
    | Some data -> Join (g, DataToMultiset data)
  in

  (* select
     http://www.w3.org/TR/sparql11-query/#sparqlSelectExpressions
  *)
  let (pv, env) =
    match q.query_proj with
      None -> (VS.empty, env)
    | Some { sel_vars = SelectAll } -> (visible_vars, env)
    | Some { sel_vars = SelectVars l } ->
       let f (acc, env) sv =
         match sv.sel_var_expr with
           None -> (VS.add sv.sel_var acc, env)
         | Some e ->
             let v = sv.sel_var in
             if VS.mem v visible_vars then
               error (Variable_already_defined v);
             (VS.add v acc, (v, e) :: env)
       in
       List.fold_left f (VS.empty, List.rev env) l
  in
  let g = List.fold_left
    (fun g (var, e) -> Extend (g, var, e))
    g (List.rev env)
  in

  (* order by *)
  let g =
    match q.query_modifier.solmod_order with
      None -> g
    | Some l -> OrderBy(g, l)
  in
  (* projection *)
  let g = Project (g, pv) in

  (* distinct / reduced *)
  let g =
    match q.query_proj with
      None -> g
    | Some s ->
      match s.sel_flag with
        None -> g
      | Some T.Distinct -> Distinct g
      | Some T.Reduced -> Reduced g
  in
  (* slice *)
  let g =
    match q.query_modifier.solmod_limoff with
      None -> g
    | Some lim ->
        match lim.limoff_offset, lim.limoff_limit with
          None, None -> g
        | o, l -> Slice (g, o, l)
  in
  g
;;

let p = Buffer.add_string;;

let string_of_var v = "?"^v.var_name
let string_of_var_or_term = function
  Rdf_sparql_types.Var v -> string_of_var v
| GraphTerm t ->
    match t with
      GraphTermIri (PrefixedName _) -> assert false
    | GraphTermIri (Iriref ir) ->
        "<"^(Rdf_uri.string ir.ir_iri)^">"
    | GraphTermLit lit
    | GraphTermNumeric lit
    | GraphTermBoolean lit ->
       Rdf_term.string_of_literal lit.rdf_lit
    | GraphTermBlank bn ->
        begin
          match bn.bnode_label with
            None -> "[]"
          | Some s -> "_:"^s
        end
    | GraphTermNil -> "()"
    | GraphTermNode node -> Rdf_term.string_of_term node

let rec string_of_path = function
  Var v ->  "?"^v.var_name
| Iri ir -> "<"^(Rdf_uri.string ir.ir_iri)^">"
| Inv p -> "(^"^(string_of_path p)^")"
| Alt (p1, p2) -> "("^(string_of_path p1)^" | "^(string_of_path p2)^")"
| Seq (p1, p2) -> "("^(string_of_path p1)^" / "^(string_of_path p2)^")"
| ZeroOrOne p -> "("^(string_of_path p)^"?)"
| ZeroOrMore p -> "("^(string_of_path p)^"*)"
| OneOrMore p -> "("^(string_of_path p)^"+)"
| NPS l -> "<nps>"

let string_of_triple (x, path, y) =
  (string_of_var_or_term x) ^ " " ^
  (string_of_path path) ^ " " ^
  (string_of_var_or_term y)

let print_triple mg b t =
  p b mg ;
  p b (string_of_triple t);
  p b " .\n"

let print_triples mg b l = List.iter (print_triple mg b) l

let print_expr = Rdf_sparql_print.print_expression ;;

let print_group_condition b = function
  GroupBuiltInCall c -> p b "GroupBuiltInCall _"
| GroupFunctionCall c -> p b "GroupFunctionCall _"
| GroupVar gv ->
    match gv.grpvar_expr, gv.grpvar with
      None, None -> assert false
    | Some e, None -> print_expr b e
    | None, Some v -> p b (string_of_var v)
    | Some e, Some v -> print_expr b e; p b (" as "^(string_of_var v))
;;

let print_order_cond b = function
  OrderAsc e -> p b "ASC(" ; print_expr b e ; p b ")"
| OrderDesc e -> p b "DESC(" ; print_expr b e ; p b ")"
| OrderConstr e -> p b "DESC(<constraint>)"
| OrderVar v -> p b (string_of_var v)
;;

let print_order_conds b l =
  List.iter
   (fun c -> print_order_cond b c; p b ", ")
   l
;;

let rec print mg b = function
| BGP triples ->
    let mg2 = mg ^ "  " in
    p b (mg^"BGP(\n");
    print_triples mg2 b triples;
    p b (mg^")")
| Join (a1, a2) ->
    p b (mg^"Join(\n");
    let mg2 = mg ^ "  " in
    print mg2 b a1;
    p b ",\n" ;
    print mg2 b a2;
    p b ")"
| LeftJoin (a1, a2, l) ->
    p b (mg^"LeftJoin(\n");
    let mg2 = mg ^ "  " in
    print mg2 b a1;
    p b ",\n" ;
    print mg2 b a2;
    p b (",\n"^mg2^"<filters>") ;
    p b ")"
| Filter (a, l) ->
    let mg2 = mg ^ "  " in
    p b (mg^"Filter(\n");
    print mg2 b a;
    p b (",\n"^mg2^"<filters>");
    p b ")"
| Union (a1, a2) ->
    let mg2 = mg ^ "  " in
    p b (mg^"Union(\n");
    print mg2 b a1;
    p b ",\n" ;
    print mg2 b a2;
    p b ")"
| Graph (vi, a) ->
    let mg2 = mg ^ "  " in
    p b (mg^"Graph(");
    Rdf_sparql_print.print_var_or_iri b vi;
    p b ",\n";
    print mg2 b a;
    p b ")"
| Extend (a, v, e) ->
    let mg2 = mg ^ "  " in
    p b (mg^"Extend(\n");
    print mg2 b a;
    p b (",\n"^mg2^(string_of_var v)^", ");
    print_expr b e;
    p b ")"
| Minus (a1, a2) ->
    let mg2 = mg ^ "  " in
    p b (mg^"Minus(\n");
    print mg2 b a1;
    p b ",\n" ;
    print mg2 b a2;
    p b ")"
| ToMultiset a ->
    p b (mg^"ToMultiset(\n");
    print (mg^"  ") b a;
    p b ")"
| DataToMultiset d ->
    p b (mg^"DataToMultiset(d)")
| Group (l, a) ->
    p b (mg^"Group([");
    List.iter (fun gc -> print_group_condition b gc; p b " ; ") l;
    p b "],\n";
    print (mg^"  ") b a;
    p b ")"
| Aggregation agg ->
    p b (mg^"Aggregation(<agg>)")
| AggregateJoin (a, l) ->
    let mg2 = mg ^ "  " in
    p b (mg^"AggregateJoin(\n");
    print mg2 b a ;
    List.iter (fun a -> p b ",\n"; print mg2 b a) l;
    p b ("\n"^mg^")")
| Project (a, set) ->
    let mg2 = mg ^ "  " in
    p b (mg^"Project(\n");
    print mg2 b a ;
    p b (",\n"^ mg2 ^ "{");
    Rdf_sparql_types.VarSet.iter
      (fun v -> p b (v.var_name^" ; ")) set;
    p b ("}\n"^mg^")")
| Distinct a ->
    p b (mg^"Distinct(\n");
    print (mg^"  ") b a;
    p b ")"
| Reduced a ->
    p b (mg^"Reduced(\n");
    print (mg^"  ") b a;
    p b ")"
| Slice (a, off, lim) ->
    let mg2 = mg ^ "  " in
    p b (mg^"Slice(\n");
    print mg2 b a ;
    p b (",\n"^mg2);
    p b (match off with None -> "NONE" | Some n -> string_of_int n);
    p b ", ";
    p b (match lim with None -> "NONE" | Some n -> string_of_int n);
    p b ")"
| OrderBy (a, l) ->
    let mg2 = mg ^ "  " in
    p b (mg^"OrderBy(\n");
    print mg2 b a;
    p b (",\n"^mg2);
    print_order_conds b l;
    p b ")"

let string_of_algebra a =
  let b = Buffer.create 256 in
  print "" b a;
  Buffer.contents b
;;

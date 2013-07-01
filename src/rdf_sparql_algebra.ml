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


type query =
    {
      query_proj : select_clause option ;
      query_where : group_graph_pattern ;
      query_modifier : solution_modifier ;
      query_values : values_clause option ;
    }

type filter = constraint_


type grouping =
  | Implicit_grouping
  | Explicit_grouping of group_condition list

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

type triple= var_or_term * path * var_or_term

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
  | Agregation
  | AgregateJoin

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
    { var_loc = Rdf_sparql_types.dummy_loc ; var_name = label }
;;

let path_iri_first = Iri { ir_loc = T.dummy_loc ; ir_iri = Rdf_rdf.rdf_first } ;;
let path_iri_rest =  Iri { ir_loc = T.dummy_loc ; ir_iri = Rdf_rdf.rdf_rest } ;;
let iri_nil =  T.Iriref { ir_loc = T.dummy_loc ; ir_iri = Rdf_rdf.rdf_nil } ;;
let iriref_type = { ir_loc = T.dummy_loc ; ir_iri = Rdf_rdf.rdf_type } ;;
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
        query_values = Some t.subsel_values ;
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
  false (* FIXME: check for implicit grouping *)

and translate_query_level q =
  let g = translate_ggp q.query_where in
  let g =
    match q.query_modifier.solmod_group with
      [] ->
       if has_implicit_grouping q then
         Group([], g)
       else
         g
   | group_conds -> Group (group_conds, g)
  in
  let g =
    match g with
      Group (conds, g) ->
            assert false
    | _ -> g
  in
      assert false

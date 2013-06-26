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



type filter = constraint_

type path =
    | PVar of var_or_term
    | Iri of iriref
    | Inv of path
    | Alt of path * path
    | Seq of path * path
    | ZeroOrMore of path
    | OneOrMore of path
    | ZeroOrOne of path
    | NPS of iri list

type triple_path = var_or_term * path * var_or_term

type algebra =
  | BGP of triple_path list
  | Join of algebra * algebra
  | LeftJoin of algebra * algebra * filter list
  | Filter of algebra * filter list
  | Union of algebra list
  | Graph of var_or_iri * algebra
  | Extend of algebra * var * expression
  | Minus of algebra * algebra
  | ToMultiset of algebra
  | DataToMultiset of datablock

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

let iri_first = Iri { ir_loc = T.dummy_loc ; ir_iri = Rdf_rdf.rdf_first };;
let iri_rest = Iri { ir_loc = T.dummy_loc ; ir_iri = Rdf_rdf.rdf_rest };;
let iri_nil =  Iri { ir_loc = T.dummy_loc ; ir_iri = Rdf_rdf.rdf_nil };;
(*
let rec build_triples_path subject acc prop_list_path =


and build_triples_path_obj_list subject prop acc obj_list =
  let f = function
  | GraphNodePathVT v_or_t -> (subject, prop, v_or_t) :: acc
  | GraphNodePathTriples triples_node_path ->
      let v = PVar (fresh_var ()) in
      let acc = (subject, prop, v) :: acc in
      match triples_node_path with
      | TNodePathCollection l ->
          build_triples_path_collection acc v l
      | TNodePathBlank l ->
          build_triples_path v acc l

and build_triples_path_collection acc subject = function
    | [] -> acc
    | h :: q ->
        let acc = build_triples_path subject (Iri iri_first) acc [h] in
        match q with
          [] ->
            (subject, Iri iri_rest, T.GraphTermIri (T.Iriref iri_nill)) :: acc
        | _ ->
            let v = PVar (fresh_var()) in
            let acc = (subject, Iri iri_rest, v) :: acc in
            build_triples_path_collection acc v q
;;

(** Translate group graph pattern as explained here:
  http://www.w3.org/TR/sparql11-query#convertGraphPattern *)
let rec translate_ggp = function
  SubSelect t -> ToMultiset (translate_subselect t)
| GGPSub t -> translate_ggp_sub t

and translate_subselect t =
  translate_ggp t.subsel_where

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
  List.flatten (List.map translate_triples_same_subject_path t.triples)

and translate_union l =
  match l with
    [] -> BGP []
  | [g] -> g
  | h :: q -> List.fold_left (fun acc g -> Union(acc, translate_ggp g)) h q

and translate_service s = failwith "SPARQL algebra: translate_service not implemented"

and translate_inline_data d = DataToMultiset d

and translate_triples_same_subject_path t =
  match t with
    T.TriplesPathVar p ->



*)
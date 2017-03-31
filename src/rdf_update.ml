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

open Rdf_sparql_types;;
open Rdf_graph;;
open Rdf_sparql_ms;;

let dbg = Rdf_misc.create_log_fun
  ~prefix: "Rdf_update"
    "RDF_UPDATE_DEBUG_LEVEL"
;;

module Bm = Rdf_graph.Bid_map

let get_bnode g bm id =
  let id = Rdf_term.blank_id_of_string id in
  try
    let id = Bm.find id bm in
    (id, bm)
  with
    Not_found ->
      let id2 = g.new_blank_id () in
      let bm = Bm.add id id2 bm in
      (id2, bm)

let var_or_term_apply_sol ~map_blanks g sol bnode_map = function
  Rdf_sparql_types.Var v ->
    (
     try
       let node = Rdf_sparql_ms.mu_find_var v sol in
       match node with
         Rdf_term.Blank_ label ->
           if map_blanks then
             begin
               let label = Rdf_term.string_of_blank_id label in
               let (label, bnode_map) = get_bnode g bnode_map label in
               (Rdf_term.blank_ label, bnode_map)
             end
           else
             (node, bnode_map)
       | Rdf_term.Blank -> assert false
       | node -> (node, bnode_map)
     with Not_found ->
       failwith ("Unbound variable "^v.var_name)
    )
| Rdf_sparql_types.GraphTerm t ->
    match t with
    | GraphTermIri (PrefixedName _) -> assert false
    | GraphTermIri (Iriref _) -> assert false
    | GraphTermIri (Iri i) -> (Rdf_term.Iri (i.iri_iri), bnode_map)
    | GraphTermLit lit
    | GraphTermNumeric lit
    | GraphTermBoolean lit -> (Rdf_term.Literal lit.rdf_lit, bnode_map)
    | GraphTermBlank { bnode_label = None }
    | GraphTermNil ->
       let label = g.new_blank_id () in
       (Rdf_term.blank_ label, bnode_map)
    | GraphTermBlank { bnode_label = Some label } ->
        if map_blanks then
          let (label, bnode_map) = get_bnode g bnode_map label in
          (Rdf_term.blank_ label, bnode_map)
        else
          (Rdf_term.blank label, bnode_map)
    | GraphTermNode _ -> assert false
;;

let apply_solution_to_graph ~map_blanks apply graph template =
  let triples =
    List.fold_left
      Rdf_sparql_algebra.translate_triples_same_subject_path [] template
  in
  dbg ~level: 2
    (fun () -> "construct "^(string_of_int (List.length triples))^" triple(s) per solution");
  let build_triple sol (triples, bnode_map) (sub, path, obj) =
    try
      let pred =
        match path with
          Rdf_sparql_algebra.Var v -> Rdf_sparql_types.Var v
        | Rdf_sparql_algebra.Iri iri ->
            Rdf_sparql_types.GraphTerm
              (Rdf_sparql_types.GraphTermIri (Rdf_sparql_types.Iri iri))
        | _ -> failwith "Invalid predicate spec in template"
      in
      let (sub, bnode_map) =
        let (node, bnode_map) =
          var_or_term_apply_sol ~map_blanks graph sol bnode_map sub
        in
        match node with
          Rdf_term.Literal _ -> failwith "Invalid subject (literal)"
        | _ -> (node, bnode_map)
      in
      let (pred, bnode_map) =
        let (node, bnode_map) =
          var_or_term_apply_sol ~map_blanks graph sol bnode_map pred
        in
        match node with
        | Rdf_term.Iri iri -> (iri, bnode_map)
        | Rdf_term.Literal _ -> failwith "Invalid predicate (literal)"
        | Rdf_term.Blank | Rdf_term.Blank_ _ -> failwith "Invalid predicate (blank)"
      in
      let (obj, bnode_map) =
        var_or_term_apply_sol ~map_blanks graph sol bnode_map obj
      in
      ((sub, pred, obj) :: triples, bnode_map)
    with
      e ->
        dbg ~level: 2 (fun _ -> Printexc.to_string e);
        (triples, bnode_map)
  in
  let f sol =
    (*Rdf_sparql_ms.SMap.iter
      (fun name term -> print_string (name^"->"^(Rdf_term.string_of_node term)^" ; "))
      sol.Rdf_sparql_ms.mu_bindings;
    print_newline();
    *)
    let (triples,_) =
      List.fold_left (build_triple sol) ([], Bm.empty) triples
    in
    List.iter (apply graph) triples
  in
  f
;;

let add_solution_to_graph =
  apply_solution_to_graph ~map_blanks: true
    (fun g (sub,pred,obj) -> g.add_triple ~sub ~pred ~obj)

let del_solution_from_graph =
  apply_solution_to_graph ~map_blanks: false
    (fun g (sub,pred,obj) -> g.rem_triple ~sub ~pred ~obj)

let on_quad_data f g ?(mu=Rdf_sparql_ms.mu_0) qd =
  (match qd.quads_list with
   | _::_ -> dbg ~level:1
       (fun () -> "Insertion of GRAPH ... { triples } not implemented yet")
   | [] -> ()
  );
  match qd.quads_triples with
    None -> ()
  | Some template -> f g template mu

let insert_data ~graph qd =
  on_quad_data add_solution_to_graph graph qd;
  true

let delete_data ~graph qd =
  on_quad_data del_solution_from_graph graph qd;
  true

let delete_where ~graph qd = false
let modify ~graph qd = false
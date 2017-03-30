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
  ~prefix: "Rdf_sparql_update"
    "RDF_SPARQL_UPDATE_DEBUG_LEVEL"
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

let term_of_graph_term g mu bm = function
  GraphTermIri (Iri { iri_iri = i }) -> (Rdf_term.Iri i, bm)
| GraphTermIri _ -> assert false
| GraphTermLit lit
| GraphTermNumeric lit
| GraphTermBoolean lit -> (Rdf_term.Literal lit.rdf_lit, bm)
| GraphTermBlank { bnode_label = None } -> assert false
| GraphTermBlank { bnode_label = Some id } ->
    begin
      let (id, bm) = get_bnode g bm id in
      (Rdf_term.blank_ id, bm)
    end
| GraphTermNil -> (Rdf_term.Iri Rdf_rdf.nil, bm)
| GraphTermNode t -> (t, bm)

let term_of_vt g mu bm = function
  Var v -> (mu_find_var v mu, bm)
| GraphTerm gt -> term_of_graph_term g m bm

let insert_triples_same_subject env g = function
  TriplesVar (vt,propol_l) ->
    insert
let insert_triples_template g mu bm l =
  List.iter (insert_triples_same_subject env g) l

let insert_quad_data g ?mu qd =
  let bm = Bm.empty in
  (match qd.quads_list with
    _::_ -> dbg ~level:1
       (fun () -> "Insertion of GRAPH ... { triples } not implemented yet")
  );
  insert_triples g mu bm qd.quads_triples


let insert_data ~graph qd = false
let delete_data ~graph qd = false
let delete_where ~graph qd = false
let modify ~graph qd = false
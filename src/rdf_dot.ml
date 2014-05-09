(*********************************************************************************)
(*                OCaml-RDF                                                      *)
(*                                                                               *)
(*    Copyright (C) 2012-2014 Institut National de Recherche en Informatique     *)
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

open Rdf_term;;

module Irimap = Rdf_iri.Irimap;;
module SSet = Rdf_types.SSet;;

let apply_namespaces namespaces iri =
  let len_iri = String.length iri in
  let rec iter = function
    [] -> ("",iri)
  | (pref,ns) :: q ->
      let len = String.length ns in
      if len <= len_iri && String.sub iri 0 len = ns then
        (pref, String.sub iri len (len_iri - len))
      else
        iter q
  in
  iter namespaces
;;

let build_namespaces ?(namespaces=[]) g =
  let l = (g.Rdf_graph.namespaces ()) @ namespaces in
  let f (map, set) (iri, pref) =
    try
      ignore(Irimap.find iri map);
      (* this iri already has a prefix, ignore this association *)
      (map, set)
    with Not_found ->
        if SSet.mem pref set then
          failwith (Printf.sprintf "%S is already the prefix of another namespace." pref)
        else
          (
           let map = Irimap.add iri pref map in
           let set = SSet.add pref set in
           (map, set)
          )
  in
  let (map, _) = List.fold_left f (Irimap.empty, SSet.empty) l in
  Irimap.fold (fun iri s acc -> (s, Rdf_iri.string iri) :: acc) map []
;;

let dot_of_graph ?namespaces ?href ?iri g =
  let namespaces = build_namespaces ?namespaces g in
  let b = Buffer.create 256 in
  Buffer.add_string b "digraph g {\nrankdir=LR;\nfontsize=10;\n";
  let triples =
    match iri with
      None -> g.Rdf_graph.find ()
    | Some iri ->
        let node = Rdf_term.Iri iri in
        let to_iri = g.Rdf_graph.find ~sub: node () in
        let from_iri = g.Rdf_graph.find ~obj: node () in
        to_iri @ from_iri
  in
  let label node =
    match node with
      Iri iri ->
         let iri = Rdf_iri.string iri in
         let (pref,s) = apply_namespaces namespaces iri in
         begin
          match pref with
            "" -> s
          | _ -> pref ^ ":" ^ s
         end
    | Literal lit ->
        lit.lit_value
          ^ (match lit.lit_language with None -> "" | Some s -> "^"^s)
          ^ (match lit.lit_type with None -> "" | Some iri -> "@"^(Rdf_iri.string iri))
    | Blank_ _ | Blank -> ""
  in
  let id node =
    let s =
      match node with
        Iri iri -> Rdf_iri.string iri
      | Blank_ id -> "b"^(string_of_blank_id id)
      | Literal lit ->
          lit.lit_value
            ^ "^" ^ (match lit.lit_language with None -> "" | Some s -> s)
            ^ "@" ^ (match lit.lit_type with None -> "" | Some iri -> Rdf_iri.string iri)
      | Blank -> assert false
    in
    "N" ^ (Digest.to_hex (Digest.string s))
  in
  let f set (sub, pred, obj) =
    Printf.bprintf b "%s -> %s [label=%S];\n" (id sub) (id obj) (label (Iri pred));
    Rdf_term.TSet.add sub (Rdf_term.TSet.add obj set)
  in
  let set = List.fold_left f Rdf_term.TSet.empty triples in
  let f_node node =
    Printf.bprintf b "%s [ label=%S %s];\n" (id node) (label node)
      (match href with
         None -> ""
       | Some f ->
           match f node with
             None -> ""
           | Some s -> ", href=\""^s^"\""
      )
  in
  Rdf_term.TSet.iter f_node set;
  Buffer.add_string b "}\n";
  Buffer.contents b
;;

let dot_of_iri ?namespaces ?href g iri = dot_of_graph ?namespaces ?href ~iri g;;
let dot_of_graph ?namespaces ?href g = dot_of_graph ?namespaces ?href g;;

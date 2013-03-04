(*********************************************************************************)
(*                OCaml-RDF                                                      *)
(*                                                                               *)
(*    Copyright (C) 2012 Institut National de Recherche en Informatique          *)
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

(** Dump in graphviz format. *)

open Rdf_node;;

module Node_set = Set.Make(Rdf_node.Ord_type)
module Urimap = Rdf_uri.Urimap;;
module SSet = Set.Make (struct type t = string let compare = Pervasives.compare end);;

let apply_namespaces namespaces uri =
  let len_uri = String.length uri in
  let rec iter = function
    [] -> ("",uri)
  | (pref,ns) :: q ->
      let len = String.length ns in
      if len <= len_uri && String.sub uri 0 len = ns then
        (pref, String.sub uri len (len_uri - len))
      else
        iter q
  in
  iter namespaces
;;

let build_namespaces ?(namespaces=[]) g =
  let l = (Rdf_rdf.rdf_"", "rdf") :: (g.Rdf_graph.namespaces ()) @ namespaces in
  let f (map, set) (uri, pref) =
    try
      ignore(Urimap.find uri map);
      (* this uri already has a prefix, ignore this association *)
      (map, set)
    with Not_found ->
        if SSet.mem pref set then
          failwith (Printf.sprintf "%S is already the prefix of another namespace." pref)
        else
          (
           let map = Urimap.add uri pref map in
           let set = SSet.add pref set in
           (map, set)
          )
  in
  let (map, _) = List.fold_left f (Urimap.empty, SSet.empty) l in
  Urimap.fold (fun uri s acc -> (s, Rdf_uri.string uri) :: acc) map []
;;

let dot_of_graph ?namespaces g =
  let namespaces = build_namespaces ?namespaces g in
  let b = Buffer.create 256 in
  Buffer.add_string b "digraph g {\nrankdir=LR;\nfontsize=10;\n";
  let triples = g.Rdf_graph.find () in
  let label node =
    match node with
      Uri uri ->
         let uri = Rdf_uri.string uri in
         let (pref,s) = apply_namespaces namespaces uri in
         begin
          match pref with
            "" -> s
          | _ -> pref ^ ":" ^ s
         end
    | Literal lit ->
        lit.lit_value
          ^ (match lit.lit_language with None -> "" | Some s -> "^"^s)
          ^ (match lit.lit_type with None -> "" | Some uri -> "@"^(Rdf_uri.string uri))
    | Blank_ _ | Blank -> ""
  in
  let id node =
    let s =
      match node with
        Uri uri -> Rdf_uri.string uri
      | Blank_ id -> "b"^(string_of_blank_id id)
      | Literal lit ->
          lit.lit_value
            ^ "^" ^ (match lit.lit_language with None -> "" | Some s -> s)
            ^ "@" ^ (match lit.lit_type with None -> "" | Some uri -> Rdf_uri.string uri)
      | Blank -> assert false
    in
    "N" ^ (Digest.to_hex (Digest.string s))
  in
  let f set (sub, pred, obj) =
    match Rdf_node.Ord_type.compare pred (Uri Rdf_rdf.ordf_ns) with
      0 -> set
    | _ ->
        Printf.bprintf b "%s -> %s [label=%S];\n" (id sub) (id obj) (label pred);
        Node_set.add sub (Node_set.add obj set)
  in
  let set = List.fold_left f Node_set.empty triples in
  let f_node node =
    Printf.bprintf b "%s [ label=%S ];\n" (id node) (label node)
  in
  Node_set.iter f_node set;
  Buffer.add_string b "}\n";
  Buffer.contents b
;;
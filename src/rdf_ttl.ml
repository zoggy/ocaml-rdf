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

open Rdf_graph ;;
open Rdf_ttl_types;;

type error =
| Parse_error of int * int * string
| Unknown_namespace of string

exception Error of error

let string_of_error = function
  Parse_error (start, stop, s) ->
    Printf.sprintf "characters %d-%d: parse error on lexeme %S" start stop s
| Unknown_namespace s ->
    "Unknown namespace '" ^ s ^ "'"
;;

let uri_of_uriref ctx s =
  (*prerr_endline (Printf.sprintf "uri_of_uriref base=%s s=%S"
    (Rdf_uri.string ctx.base) s);
  *)
  let url = Rdf_uri.neturl (Rdf_uri.uri s) in
  let url = Neturl.ensure_absolute_url ~base: (Rdf_uri.neturl ctx.base) url in
  Rdf_uri.of_neturl url
;;

let uri_of_resource ctx = function
  Uriref uri -> uri_of_uriref ctx uri
| Qname (p, n) ->
    (*prerr_endline
      (Printf.sprintf "Qname(%S, %S)"
       (match p with None -> "" | Some s -> s)
         (match n with None -> "" | Some s -> s)
      );
    *)
    let p = match p with None -> "" | Some s -> s in
    begin
      let base =
        try SMap.find p ctx.prefixes
        with Not_found ->
            raise (Error (Unknown_namespace p))
      in
      match n with
        None -> base
      | Some n ->
          let uri = (Rdf_uri.string base)^n in
          Rdf_uri.uri uri
    end
;;

let rec mk_blank ctx g = function
  NodeId id ->
    let (node, gstate) = Rdf_xml.get_blank_node g ctx.gstate id in
    (node, { ctx with gstate }, g)
| Empty ->
    let node = Rdf_node.Blank_ (g.new_blank_id ()) in
    (node, ctx, g)
| PredObjs l ->
    let node = Rdf_node.Blank_ (g.new_blank_id ()) in
    let (ctx, g) = List.fold_left (insert_sub_predobj node) (ctx, g) l in
    (node, ctx, g)
| Collection [] -> (Rdf_node.Uri Rdf_rdf.rdf_nil, ctx, g)
| Collection objects ->
    let node = Rdf_node.Blank_ (g.new_blank_id ()) in
    let (ctx, g) = mk_collection ctx g node objects in
    (node, ctx, g)

and mk_collection ctx g node = function
  [] -> assert false
| h :: q ->
   let (obj, ctx, g) = mk_object_node ctx g h in
   g.add_triple ~sub: node
        ~pred: (Rdf_node.Uri Rdf_rdf.rdf_first) ~obj;
   match q with
     [] ->
        g.add_triple ~sub: node
         ~pred: (Rdf_node.Uri Rdf_rdf.rdf_rest)
         ~obj: (Rdf_node.Uri Rdf_rdf.rdf_nil);
       (ctx, g)
   | _ ->
       let obj = Rdf_node.Blank_ (g.new_blank_id ()) in
        g.add_triple ~sub: node
          ~pred: (Rdf_node.Uri Rdf_rdf.rdf_rest)
          ~obj ;
       mk_collection ctx g obj q

and mk_object_node ctx g = function
  | Obj_res res -> (Rdf_node.Uri (uri_of_resource ctx res), ctx, g)
  | Obj_blank b -> mk_blank ctx g b
  | Obj_literal (String (s, lang, typ)) ->
       let typ =
         match typ with
           None -> None
         | Some r -> Some (uri_of_resource ctx r)
       in
       let lit = Rdf_node.mk_literal ?typ ?lang s in
       (Rdf_node.Literal lit, ctx, g)

and insert_pred sub pred (ctx, g) obj =
  let (obj, ctx, g) = mk_object_node ctx g obj in
  g.add_triple ~sub ~pred ~obj;
  (ctx, g)

and insert_sub_predobj sub (ctx, g) (pred, objs) =
  let pred =
    match pred with
      Pred_res r -> Rdf_node.Uri (uri_of_resource ctx r)
    | Pred_a -> Rdf_node.Uri Rdf_rdf.rdf_type
  in
  List.fold_left (insert_pred sub pred) (ctx, g) objs

and insert_sub_predobjs ctx g sub l =
  let (sub, ctx, g) =
    match sub with
      Sub_res r -> (Rdf_node.Uri (uri_of_resource ctx r), ctx, g)
    | Sub_blank b -> mk_blank ctx g b
  in
  List.fold_left (insert_sub_predobj sub) (ctx, g) l
;;

let apply_statement (ctx, g) = function
  Directive (Prefix (s_opt, uri)) ->
    let s = match s_opt with None -> "" | Some s -> s in
    (*prerr_endline (Printf.sprintf "Directive (Prefix (%S, %s))" s uri);*)
    let uri = uri_of_uriref ctx uri in
    (*prerr_endline (Printf.sprintf "uri=%s" (Rdf_uri.string uri));*)
    let ctx = { ctx with prefixes = SMap.add s uri ctx.prefixes } in
    (ctx, g)
| Directive (Base uri) ->
    let uri = uri_of_uriref ctx uri in
    let ctx = { ctx with base = uri } in
    (ctx, g)
| Triples (subject, predobjs) ->
    insert_sub_predobjs ctx g subject predobjs
;;

let apply_statements ctx g l =
  List.fold_left apply_statement (ctx, g) l
;;

let from_lexbuf g ~base lexbuf =
  let gstate = {
      Rdf_xml.blanks = SMap.empty ;
      gnamespaces = Rdf_uri.Urimap.empty ;
    }
  in
  let ctx = {
      base = base ;
      prefixes = Rdf_ttl_types.SMap.empty ;
      gstate }
  in
  let parse = Rdf_ulex.menhir_with_ulex Rdf_ttl_parser.main Rdf_ttl_lex.main in
  let statements =
    try parse lexbuf
    with Rdf_ttl_parser.Error ->
        let (start, stop) = Ulexing.loc lexbuf in
        let lexeme = Ulexing.utf8_lexeme lexbuf in
        raise (Error (Parse_error (start, stop, lexeme)))
  in
  let (ctx, g) = apply_statements ctx g statements in
  g

let from_string g ~base s =
  let lexbuf = Ulexing.from_utf8_string s in
  from_lexbuf g ~base lexbuf
;;

let from_file g ~base file =
  let ic = open_in file in
  let lexbuf = Ulexing.from_utf8_channel ic in
  try from_lexbuf g ~base lexbuf
  with e ->
      close_in ic;
      raise e
;;

let string_of_triple ~sub ~pred ~obj =
  (Rdf_node.string_of_node sub)^" "^
  (Rdf_node.string_of_node pred)^" "^
  (Rdf_node.string_of_node obj)^" ."
;;

let to_ print g =
  List.iter (fun (sub, pred, obj) -> print (string_of_triple ~sub ~pred ~obj)) (g.find ())

let to_string g =
  let b = Buffer.create 256 in
  let print s = Buffer.add_string b (s^"\n") in
  to_ print g;
  Buffer.contents b
;;


let to_file g file =
  let oc = open_out_bin file in
  try
    let print s = output_string oc (s^"\n") in
    to_ print g;
    close_out oc
  with e ->
      close_out oc;
      raise e
;;



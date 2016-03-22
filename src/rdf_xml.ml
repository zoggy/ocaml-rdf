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

open Rdf_graph;;
open Rdf_term;;

(** {2 Using trees for XML docs}
  Code taken from Xmlm examples.
  Thanks to Xmlm, namespaces are already handled by the parser :-)
*)

type tree = E of Xmlm.tag * tree list | D of string

let in_tree i =
  let el tag childs = E (tag, childs)  in
  let data d = D d in
  try
    Xmlm.input_doc_tree ~el ~data i
  with
    Xmlm.Error ((line, col), error) ->
      let msg = Printf.sprintf "Line %d, column %d: %s"
        line col (Xmlm.error_message error)
      in
      failwith msg

let out_tree o t =
  let frag = function
  | E (tag, childs) -> `El (tag, childs)
  | D d -> `Data d
  in
  Xmlm.output_doc_tree frag o t

let apply_namespaces = Rdf_dot.apply_namespaces;;

let output_doc_tree ns ?(decl=true) dest tree =
  let map (pref, s) =
    match pref with
      "" -> apply_namespaces ns s
    | _ -> (pref, s)
  in
  let tree =
    match tree with
      D _ -> tree
    | E ((tag,atts),subs) ->
        let atts = List.fold_left
          (fun acc (((pref,s),v) as att) ->
             if pref = Xmlm.ns_xmlns then acc else att :: acc
          )
          []
          atts
        in
        let ns_atts = List.map (fun (pref,iri) -> ((Xmlm.ns_xmlns, pref), iri)) ns in
        E ((tag, ns_atts @ atts), subs)
  in
  let ns_prefix s = Some s in
  let output = Xmlm.make_output ~ns_prefix ~decl dest in
  let frag = function
  | D d -> `Data d
  | E (((pref,s),atts), childs) ->
      let (pref, s) = map (pref, s) in
      let atts = List.map
        (fun ((pref,s),v) -> (map (pref, s), v)) atts
      in
      `El (((pref,s),atts), childs)
  in
  Xmlm.output_doc_tree frag output (None, tree);
;;

let string_of_xmls namespaces trees =
  try
    let b = Buffer.create 256 in
    List.iter (output_doc_tree namespaces ~decl: false (`Buffer b)) trees;
    Buffer.contents b
  with
    Xmlm.Error ((line, col), error) ->
      let msg = Printf.sprintf "Line %d, column %d: %s"
        line col (Xmlm.error_message error)
      in
      failwith msg
;;

let xml_of_string str =
  try
    let i = Xmlm.make_input ~strip: true (`String (0, str)) in
    let (_,tree) = in_tree i in
    (*prerr_endline "parse ok";*)
    tree
  with
    Xmlm.Error ((line, col), error) ->
      let msg = Printf.sprintf "Line %d, column %d: %s\n%s"
        line col (Xmlm.error_message error) str
      in
      failwith msg
;;

let xmls_of_string str =
  (*prerr_endline "xmls_of_string";*)
  let str = "<foo__>"^str^"</foo__>" in
  match xml_of_string str with
    E ((("","foo__"),_),subs) -> subs
  | _ -> assert false
;;

let get_first_child xml tag =
  match xml with
    D _ -> None
  | E ((_,_),subs) ->
      try Some (List.find (function E ((t,_),_) -> t = tag | _ -> false) subs)
      with Not_found -> None
;;

let is_element iri (pref,loc) =
  let iri2 = Iri.of_string (pref^loc) in
  let b = Iri.compare iri iri2 = 0 in
  (*prerr_endline (Printf.sprintf "is_element %s %s: %b"
    (Iri.to_string iri) (Iri.to_string iri2) b);*)
   b
;;


(** {2 Input} *)

module SMap = Rdf_types.SMap;;
module Irimap = Iri.Map

type state =
  { subject : Rdf_term.term option ;
    predicate : Iri.t option ;
    xml_base : Iri.t ;
    xml_lang : string option ;
    datatype : Iri.t option ;
    namespaces : string Irimap.t ;
  }

type global_state =
  {
    blanks : Rdf_term.blank_id SMap.t ;
    gnamespaces : string Irimap.t ;
  }

exception Invalid_rdf of string
let error s = raise (Invalid_rdf s);;

let get_att att l = try Some (List.assoc att l) with Not_found -> None;;
let get_att_iri =
  let rec iter pred = function
    [] -> None
  | (x,v) :: q ->
    if pred x then Some v else iter pred q
  in
  fun iri l -> iter (is_element iri) l
;;

(*
let abs_iri state iri =
  prerr_endline (Printf.sprintf "resolve base=%s  iri=%s"
    (Iri.to_string state.xml_base) (Iri.ref_to_string iri));

  let iri = Iri.resolve ~base: state.xml_base iri in
  prerr_endline (Printf.sprintf "=> %s" (Iri.to_string iri));
  iri
*)

let abs_iri state iri =
  (*prerr_endline (Printf.sprintf "resolve base=%s  iri=%s"
   (Iri.to_string state.xml_base) (Iri.to_string iri));*)
  let iri =
     match Iri.is_relative iri with
       false -> iri
     | true ->
         let str = (Iri.to_string state.xml_base)^(Iri.to_string iri) in
         Iri.of_string str
   in
   (*prerr_endline (Printf.sprintf "=> %s" (Iri.to_string iri));*)
   iri

let set_xml_base state = function
  D _ -> state
| E ((_,atts),_) ->
    match get_att (Xmlm.ns_xml, "base") atts with
      None -> state
    | Some s ->
        let r = Iri.of_string s in
        let xml_base = abs_iri state r in
        { state with xml_base }
;;
let set_xml_lang state = function
  D _ -> state
| E ((_,atts),_) ->
    match get_att (Xmlm.ns_xml, "lang") atts with
      None -> state
    | Some s ->
        (*prerr_endline ("setting lang to "^s);*)
        { state with xml_lang = Some s }
;;
let set_namespaces gstate state = function
  D _ -> (gstate, state)
| E ((_,atts),_) ->
    let f (gstate, state) ((pref,s),v) =
      if pref = Xmlm.ns_xmlns then
        begin
          let iri = Iri.of_string v in
          let gstate = { gstate with gnamespaces = Irimap.add iri s gstate.gnamespaces } in
          let state = { state with namespaces = Irimap.add iri s state.namespaces } in
          (gstate, state)
        end
      else
        (gstate, state)
    in
    List.fold_left f (gstate, state) atts
;;

let update_state gstate state t =
  set_namespaces gstate (set_xml_lang (set_xml_base state t) t) t;;

let get_blank_node g gstate id =
  try (Blank_ (SMap.find id gstate.blanks), gstate)
  with Not_found ->
    (*prerr_endline (Printf.sprintf "blank_id for %s not found, forging one" id);*)
    let bid = g.new_blank_id () in
    let gstate = { gstate with blanks = SMap.add id bid gstate.blanks } in
    (Blank_ bid, gstate)

let rec input_node g state gstate t =
  let (gstate, state) = update_state gstate state t in
  match t with
    D s when state.predicate = None ->
      let msg = Printf.sprintf "Found (Data %S) with no current predicate." s in
      error msg
  | D s ->
      let obj = Rdf_term.term_of_literal_string ?typ: state.datatype ?lang: state.xml_lang s in
      let sub = match state.subject with None -> assert false | Some s -> s in
      let pred = match state.predicate with None -> assert false | Some u -> u in
      g.add_triple ~sub ~pred ~obj;
      gstate
  | E (((pref,s), atts), children) ->
      let (node, gstate) =
        match get_att_iri Rdf_rdf.about atts with
          Some s -> (Iri (abs_iri state (Iri.of_string s)), gstate)
        | None ->
            match get_att_iri Rdf_rdf.id atts with
              Some id -> (Iri (Iri.of_string ((Iri.to_string state.xml_base)^"#"^id)), gstate)
            | None ->
                match get_att_iri Rdf_rdf.nodeID atts with
                  Some id -> get_blank_node g gstate id
                | None -> (Blank_ (g.new_blank_id ()), gstate)
      in
      begin
        match state.subject, state.predicate with
          Some sub, Some pred ->
            g.add_triple ~sub ~pred ~obj: node
        | _ -> ()
      end;
      let state = { state with subject = Some node ; predicate = None } in
      (* add a type arc if the node is not introduced with rdf:Description *)
      if not (is_element Rdf_rdf.description (pref,s)) then
        begin
          let type_iri = Iri.of_string (pref^s) in
          g.add_triple ~sub: node ~pred: Rdf_rdf.type_ ~obj: (Iri type_iri)
        end;
      (* all remaining attributes define triples with literal object values *)
      let f ((pref, s), v) =
        if pref <> Xmlm.ns_xml && pref <> Xmlm.ns_xmlns then
          begin
            let iri_prop = Iri.of_string (pref^s) in
            if not (List.exists (Iri.equal iri_prop) [ Rdf_rdf.about ; Rdf_rdf.id; Rdf_rdf.nodeID ]) then
              begin
                let obj = Rdf_term.term_of_literal_string ?lang: state.xml_lang v in
                g.add_triple ~sub: node ~pred: iri_prop ~obj
              end
          end
      in
      List.iter f atts;
      let (gstate, _) = List.fold_left (input_prop g state) (gstate, 1) children in
      gstate

(* FIXME: handle rdf:ID *)
and input_prop g state (gstate, li) t =
  let (gstate, state) = update_state gstate state t in
  match t with
    D s ->
      let msg = Printf.sprintf "Found (Data %S) but expected a property node." s in
      error msg
  | E (((pref,s),atts),children) ->
      let sub = match state.subject with None -> assert false | Some sub -> sub in
      let prop_iri = Iri.of_string (pref^s) in
      let (prop_iri, li) =
        if Iri.equal prop_iri Rdf_rdf.li then
          (Rdf_rdf.n li, li + 1)
        else
          (prop_iri, li)
      in
      match get_att_iri Rdf_rdf.resource atts with
        Some s ->
          let iri = Iri.of_string s in
          let obj = Iri (abs_iri state iri) in
          g.add_triple ~sub ~pred: prop_iri ~obj ;
          (gstate, li)
      | None ->
          match get_att_iri Rdf_rdf.nodeID atts with
            Some id ->
              let (obj, gstate) = get_blank_node g gstate id in
              g.add_triple ~sub ~pred: prop_iri ~obj ;
              (gstate, li)
          | None ->
          match get_att_iri Rdf_rdf.parseType atts with
            Some "Literal" ->
              let xml = string_of_xmls
                (Irimap.fold (fun iri s acc -> (s, Iri.to_string iri) :: acc) state.namespaces [])
                children
              in
              let obj = Rdf_term.term_of_literal_string ~typ: Rdf_rdf.dt_XMLLiteral xml in
              g.add_triple ~sub ~pred: prop_iri ~obj;
              (gstate, li)
          | Some "Resource" ->
              begin
                 let node = Blank_ (g.new_blank_id ()) in
                 g.add_triple ~sub ~pred: prop_iri ~obj: node ;
                 let state = { state with subject = Some node ; predicate = None } in
                 List.fold_left (input_prop g state) (gstate, 1) children
              end
          | Some "Collection" ->
              begin
                let rec f (gstate, previous) = function
                  [] -> assert false
                | first :: rest ->
                   let state = { state with
                     subject = Some previous ;
                     predicate = Some Rdf_rdf.first }
                   in
                   let gstate = input_node g state gstate first in
                   match rest with
                     [] -> g.add_triple ~sub: previous
                        ~pred: Rdf_rdf.rest ~obj: (Iri Rdf_rdf.nil);
                        (gstate, previous)
                   | _ ->
                      let blank = Rdf_term.Blank_ (g.new_blank_id ()) in
                      g.add_triple ~sub: previous ~pred: Rdf_rdf.rest ~obj: blank;
                      f (gstate, blank) rest
                in
                let gstate =
                  match children with
                    [] -> gstate
                  | _ ->
                    let blank = Rdf_term.Blank_ (g.new_blank_id ()) in
                    g.add_triple ~sub ~pred: prop_iri ~obj: blank;
                    fst (f (gstate, blank) children)
                in
                (gstate, li)
              end
          | Some s -> error (Printf.sprintf "Unknown parseType %S" s)
          | None ->
              match get_att_iri Rdf_rdf.datatype atts, children with
              | Some s, [D lit] ->
                  let typ = abs_iri state (Iri.of_string s) in
                  let obj = Rdf_term.term_of_literal_string ~typ ?lang: state.xml_lang lit in
                  g.add_triple ~sub ~pred: prop_iri ~obj;
                  (gstate, li)
              | Some s, _ ->
                  let msg = Printf.sprintf "Property %S with datatype %S but no data"
                    (Iri.to_string prop_iri) s
                  in
                  error msg
              | None, _ ->
                  (* if we have other attributes than the ones filtered above, they
                    are property relations, with ommited blank nodes *)
                  let pred ((pref,s),v) =
                    pref <> Xmlm.ns_xml && pref <> Xmlm.ns_xmlns &&
                    (let iri = Iri.of_string (pref^s) in not (Iri.equal iri Rdf_rdf.id))
                  in
                  match List.filter pred atts with
                    [] ->
                      let state = { state with predicate = Some prop_iri } in
                      let gstate = List.fold_left (input_node g state) gstate children in
                      (gstate, li)
                  | l ->
                      let node = Blank_ (g.new_blank_id ()) in
                      g.add_triple ~sub ~pred: prop_iri ~obj: node ;
                      let f ((pref,s),lit) =
                        let obj = Rdf_term.term_of_literal_string ?lang: state.xml_lang lit in
                        let iri_prop = Iri.of_string (pref^s) in
                        g.add_triple ~sub: node ~pred: iri_prop ~obj
                      in
                      List.iter f l;
                      (gstate, li)
;;

let input_tree g ?(base=g.Rdf_graph.name()) t =
  let state = {
      subject = None ; predicate = None ;
      xml_base = base ; xml_lang = None ;
      datatype = None ; namespaces = Irimap.empty ;
    }
  in
  let gstate = { gnamespaces = Irimap.empty ; blanks = SMap.empty } in
  let (gstate, state) = update_state gstate state t in
  let gstate =
    match t with
      D _ -> assert false
    | E ((e,_),children) when is_element Rdf_rdf._RDF e ->
        List.fold_left (input_node g state) gstate children
    | t -> input_node g state gstate t
  in
  (* add namespaces *)
  let add_ns iri prefix = g.add_namespace iri prefix in
  Irimap.iter add_ns gstate.gnamespaces
;;

let from_input g ?base i =
  let (_, tree) = in_tree i in
  input_tree g ?base tree
;;

let from_xml = input_tree;;

let from_string g ?base s =
  let i = Xmlm.make_input ~strip: true (`String (0, s)) in
  from_input g ?base i
;;

let from_file g ?base file =
  let ic = open_in file in
  let i = Xmlm.make_input ~strip: true (`Channel ic) in
  let (_,tree) =
    try let t = in_tree i in close_in ic; t
    with e -> close_in ic; raise e
  in
  input_tree g ?base tree
;;

(** {2 Output} *)

let output g =
  let xml_prop pred_iri obj =
    let (atts, children) =
      match obj with
      | Iri iri -> ([("", Iri.to_string Rdf_rdf.resource), Iri.to_string iri], [])
      | Blank_ id -> ([("", Iri.to_string Rdf_rdf.nodeID), Rdf_term.string_of_blank_id id], [])
      | Blank -> assert false
      | Literal lit ->
          let (atts, subs) =
            match lit.lit_type with
              None -> ([], [D lit.lit_value])
            | Some iri when Iri.equal iri Rdf_rdf.dt_XMLLiteral ->
                let subs = xmls_of_string lit.lit_value in
                (
                 [("",Iri.to_string Rdf_rdf.parseType), "Literal"],
                 subs
                )
            | Some iri ->
                (
                 [("",Iri.to_string Rdf_rdf.datatype), Iri.to_string iri],
                 [D lit.lit_value]
                )
          in
          let atts = atts @
            (match lit.lit_language with
               None -> []
             | Some lang -> [(Xmlm.ns_xml, "lang"), lang])
          in
          (atts, subs)
    in
    E ((("",Iri.to_string pred_iri),atts),children)
  in
  let f_triple acc (sub, pred, obj) =
    let atts =
      match sub with
        Iri iri -> [("", Iri.to_string Rdf_rdf.about), Iri.to_string iri]
      | Blank_ id -> [("", Iri.to_string Rdf_rdf.nodeID), Rdf_term.string_of_blank_id id]
      | Blank -> assert false
      | Literal _ -> assert false
    in
    let xml_prop = xml_prop pred obj in
    (E ((("",Iri.to_string Rdf_rdf.description), atts), [xml_prop]) :: acc)
  in
  let xmls = List.fold_left f_triple [] (g.find ()) in
  E ((("", Iri.to_string Rdf_rdf._RDF),[]), xmls)


let to_ ?namespaces g dest =
  let namespaces = Rdf_dot.build_namespaces ?namespaces g in
  try
    let tree = output g in
    output_doc_tree namespaces ~decl: true dest tree
  with
    Xmlm.Error ((line, col), error) ->
      let msg = Printf.sprintf "Line %d, column %d: %s"
        line col (Xmlm.error_message error)
      in
      failwith msg
;;

let to_string ?namespaces g =
  let buf = Buffer.create 256 in
  let dest = `Buffer buf in
  to_ ?namespaces g dest;
  Buffer.contents buf
;;

let to_file ?namespaces g file =
  let oc = open_out file in
  try
    to_ ?namespaces g (`Channel oc);
    close_out oc
  with e ->
      close_out oc ; raise e
;;



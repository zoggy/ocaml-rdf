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

let rdfs = Rdf_iri.iri "http://www.w3.org/2000/01/rdf-schema#";;
let rdfs_ = Rdf_iri.append rdfs;;

let rdfs_Class = rdfs_"Class" ;;

let rdfs_comment = rdfs_"comment" ;;

let rdfs_Container = rdfs_"Container" ;;

let rdfs_ContainerMembershipProperty = rdfs_"ContainerMembershipProperty" ;;

let rdfs_Datatype = rdfs_"Datatype" ;;

let rdfs_domain = rdfs_"domain" ;;

let rdfs_isDefinedBy = rdfs_"isDefinedBy" ;;

let rdfs_label = rdfs_"label" ;;

let rdfs_Literal = rdfs_"Literal" ;;

let rdfs_member = rdfs_"member" ;;

let rdfs_range = rdfs_"range" ;;

let rdfs_Resource = rdfs_"Resource" ;;

let rdfs_seeAlso = rdfs_"seeAlso" ;;

let rdfs_subClassOf = rdfs_"subClassOf" ;;

let rdfs_subPropertyOf = rdfs_"subPropertyOf" ;;

let add_label g iri ?lang s =
  let obj = Rdf_term.term_of_literal_string ?lang s in
  g.Rdf_graph.add_triple
    ~sub: (Rdf_term.Iri iri)
    ~pred: rdfs_label
    ~obj
;;

let add_comment g iri ?lang s =
let obj = Rdf_term.term_of_literal_string ?lang s in
  g.Rdf_graph.add_triple
    ~sub: (Rdf_term.Iri iri)
    ~pred: rdfs_comment
    ~obj
;;

let add_domain g iri dom =
  g.Rdf_graph.add_triple
    ~sub: (Rdf_term.Iri iri)
    ~pred: rdfs_domain
    ~obj: (Rdf_term.Iri dom)
;;

let add_range g iri dom =
  g.Rdf_graph.add_triple
    ~sub: (Rdf_term.Iri iri)
    ~pred: rdfs_range
    ~obj: (Rdf_term.Iri dom)
;;

let add_more g iri (pred, obj) =
  g.Rdf_graph.add_triple ~sub: (Rdf_term.Iri iri) ~pred ~obj
;;

let property g ~label ?(label_lang=[]) ?comment ?(comment_lang=[])
  ?(domains=[]) ?(ranges=[]) ?subof ?(more=[]) iri =
  g.Rdf_graph.add_triple ~sub: (Rdf_term.Iri iri)
    ~pred: Rdf_rdf.rdf_type ~obj: (Rdf_term.Iri Rdf_rdf.rdf_Property);
  add_label g iri label ;
  List.iter (fun (s, lang) -> add_label g iri ~lang s) label_lang;
  (match comment with None -> () | Some s -> add_comment g iri s);
  List.iter (fun (s, lang) -> add_comment g iri ~lang s) comment_lang;
  List.iter (add_domain g iri) domains ;
  List.iter (add_range g iri) ranges ;
  (match subof with
    None -> ()
  | Some cl ->
       g.Rdf_graph.add_triple ~sub: (Rdf_term.Iri iri)
         ~pred: rdfs_subPropertyOf ~obj: (Rdf_term.Iri cl)
  );
  List.iter (add_more g iri) more
;;

let class_ g ~label ?(label_lang=[]) ?comment ?(comment_lang=[])
   ?subof ?(more=[]) iri =
  g.Rdf_graph.add_triple ~sub: (Rdf_term.Iri iri)
    ~pred: Rdf_rdf.rdf_type ~obj: (Rdf_term.Iri rdfs_Class);
  add_label g iri label ;
  List.iter (fun (s, lang) -> add_label g iri ~lang s) label_lang;
  (match comment with None -> () | Some s -> add_comment g iri s);
  List.iter (fun (s, lang) -> add_comment g iri ~lang s) comment_lang;
  (match subof with
    None -> ()
  | Some cl ->
       g.Rdf_graph.add_triple ~sub: (Rdf_term.Iri iri)
         ~pred: rdfs_subClassOf ~obj: (Rdf_term.Iri cl)
  );
  List.iter (add_more g iri) more
;;

let add_namespaces g =
  g.Rdf_graph.add_namespace Rdf_rdf.rdf "rdf" ;
  g.Rdf_graph.add_namespace rdfs "rdfs"
;;


  
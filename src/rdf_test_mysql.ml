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

let string_of_triple (sub, pred, obj) =
  Printf.sprintf "%s %s %s."
  (Rdf_term.string_of_term sub)
  (Iri.to_string pred)
  (Rdf_term.string_of_term obj)
;;

let main () =
  let options =
    [ "storage", "mysql" ;
      "database", "testrdf";
      "user", Sys.getenv "USER";
    ]
  in
  let g = Rdf_graph.open_graph ~options (Iri.of_string "http://hello.fr") in
  let pred = Iri.of_string "http://dis-bonjour.org" in
  let obj = Rdf_term.term_of_literal_string "youpi" in
  let sub = Rdf_term.term_of_iri_string "http://coucou0.net" in
  for i = 0 to 10 do
    g.add_triple
    ~sub: (Rdf_term.term_of_iri_string (Printf.sprintf "http://coucou%d.net" i))
    ~pred ~obj
  done;
  g.rem_triple
    ~sub: (Rdf_term.term_of_iri_string "http://coucou3.net")
    ~pred ~obj;
  let subjects = g.subjects_of ~pred ~obj in
  List.iter (fun term -> print_endline (Rdf_term.string_of_term term)) subjects;

  let b = g.exists_t (sub, pred, obj) in
  assert b;
  let b = g.exists ~sub ~obj () in
  assert b;
  let b = not (g.exists ~obj: (Rdf_term.term_of_iri_string "http://") ()) in
  assert b;
  let triples = g.find () in
  List.iter (fun t -> print_endline (string_of_triple t)) triples;

  let subjects = g.subjects () in
  List.iter (fun term -> print_endline (Rdf_term.string_of_term term)) subjects;

  let sub4 = Rdf_term.term_of_iri_string "http://coucou4.net" in
  g.transaction_start ();
  g.rem_triple ~sub: sub4 ~pred ~obj;
  assert (not (g.exists_t (sub4, pred, obj)));
  g.transaction_rollback ();
  assert (g.exists_t (sub4, pred, obj));

  g.add_namespace (Iri.of_string "http://dis-bonjour.org") "bonjour" ;
  g.add_namespace (Iri.of_string "http://coucou1.net") "coucou1" ;
  print_endline (Rdf_ttl.to_string g);
  g.rem_namespace "coucou1";
  g.rem_namespace "coucou2";
  g.set_namespaces [
    (Iri.of_string "http://coucou3.net", "coucou3");
    (Iri.of_string "http://coucou4.net", "coucou4");
  ];
  print_endline (Rdf_ttl.to_string g);
;;
let () = main();;

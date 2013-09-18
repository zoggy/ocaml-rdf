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

(** Testing Sparql parser. *)

open Rdf_sparql_types
open Rdf_sparql_algebra

let fatal s = prerr_endline s ; exit 1 ;;
let usage = Printf.sprintf "Usage: %s [options] <queries>\nwhere options are:" Sys.argv.(0);;

let print_queries = ref false;;
let eval_queries = ref false;;

(*c==v=[Misc.safe_main]=1.0====*)
let safe_main main =
  try main ()
  with
    Failure s
  | Sys_error s ->
      prerr_endline s;
      exit 1
(*/c==v=[Misc.safe_main]=1.0====*)

let eval_query ?data query =
  let base = Rdf_uri.uri "http://localhost/" in
  let graph =
    match data with
      None -> Rdf_graph.open_graph base
    | Some g -> g
  in
  let dataset = Rdf_ds.dataset graph in
  match Rdf_sparql.execute ~base dataset query with
  | Rdf_sparql.Bool true -> print_endline "true"
  | Rdf_sparql.Bool false -> print_endline "false"
  | Rdf_sparql.Graph _ -> print_endline "graph"
  | Rdf_sparql.Solutions sols ->
      let f_sol sol =
        Rdf_sparql.solution_iter
          (fun name term -> print_string (name^"->"^(Rdf_term.string_of_term term)^" ; "))
          sol;
        print_newline()
      in
      print_endline "Solutions:";
      List.iter f_sol sols
;;

let parse_query parse ?data source =
  try
    let q = parse source in
    if !print_queries then
      print_endline (Rdf_sparql.string_of_query q);
    if !eval_queries then
      eval_query ?data q
  with
    Rdf_sparql.Error e ->
      prerr_endline (Rdf_sparql.string_of_error e);
      exit 1
;;

let parse_query_string = parse_query Rdf_sparql.query_from_string;;
let parse_query_file = parse_query Rdf_sparql.query_from_file;;
let load_ttl_data = ref None;;

let files = ref [];;

let main () =
  let args = ref [] in
  Arg.parse
    [
      "-f", Arg.String (fun f -> files := f :: !files),
      "<file> read query from <file>" ;

      "-p", Arg.Set print_queries, " print back parsed queries";
      "-e", Arg.Set eval_queries, " evaluate parsed queries";

      "-d", Arg.String (fun s -> load_ttl_data := Some s),
      "file load data from turtle file";
    ]
    (fun s -> args := s :: !args)
    usage;

  let queries = List.rev !args in
  let files = List.rev !files in
  let data =
    match !load_ttl_data with
      None -> None
    | Some file ->
        let base = Rdf_uri.uri "http://localhost/" in
        let graph = Rdf_graph.open_graph base in
        try Rdf_ttl.from_file graph file; Some graph
        with
        | Rdf_ttl.Error e ->
            prerr_endline (Rdf_ttl.string_of_error e);
            exit 1
  in
  List.iter (parse_query_string ?data) queries;
  List.iter (parse_query_file ?data) files;
;;


let () = safe_main main;;

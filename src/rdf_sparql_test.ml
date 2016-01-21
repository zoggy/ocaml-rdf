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

(** Testing. *)

module C = Config_file;;

open Rdf_sparql_types;;
open Rdf_sparql;;

let verb = print_endline;;

type test_spec =
  { base : Iri.t option ;
    title : string ;
    desc : string option;
    query : string ;
    default_graph : string option ;
    named : (Iri.t * string) list ;
    options : (string * string) list ;
  }

type result = Error of string | Ok of Rdf_sparql.query_result

type test = {
  spec : test_spec ;
  result : result ;
  }

let load_file ?graph_options file =
  verb ("Loading file "^file);
  let group = new C.group in
  let base = new C.option_cp C.string_wrappers ~group ["base"] None "Base iri" in
  let title = new C.string_cp ~group ["title"] "" "Title of the test" in
  let desc = new C.string_cp ~group ["descr"] "" "Description of the test" in
  let query = new C.string_cp ~group ["query"] "" "File containing the query" in
  let default_graph = new C.string_cp ~group ["default_graph"] "" "File containing the default graph" in
  let named = new C.list_cp (C.tuple2_wrappers C.string_wrappers C.string_wrappers) ~group
    [ "named_graphs"] [] "Named graphs in the form (iri, file.ttl)"
  in
  let options = new C.list_cp
    (C.tuple2_wrappers C.string_wrappers C.string_wrappers) ~group
      [ "graph_options"] [] "Graph creation options (storage, ...)"
  in
  group#read file;
  let mk_filename =
    let path = Filename.dirname file in
    fun file ->
      if Filename.is_relative file then
        Filename.concat path file
      else
        file
  in
  let options =
    match graph_options with
      None -> options#get
    | Some s ->
       let wr = C.list_wrappers (C.tuple2_wrappers C.string_wrappers C.string_wrappers) in
       wr.C.of_raw (C.Raw.of_string s)
  in
  let named = List.map (fun (iri, s) -> (Iri.of_string iri, mk_filename s)) named#get in
  { base = Rdf_misc.map_opt Iri.of_string base#get ;
    title = title#get ;
    desc = Rdf_misc.opt_of_string desc#get ;
    query = mk_filename query#get ;
    default_graph = Rdf_misc.map_opt mk_filename (Rdf_misc.opt_of_string default_graph#get) ;
    named ;
    options ;
  }
;;

let load_ttl g base file =
  verb ("Loading graph from "^file);
  try Rdf_ttl.from_file g ~base file
  with
  | Rdf_ttl.Error e ->
      prerr_endline (Rdf_ttl.string_of_error e);
      exit 1
;;

let mk_dataset spec =
  let base =
    match spec.base with
      None -> Iri.of_string "http://localhost/"
    | Some iri -> iri
  in
  let default =
    let g = Rdf_graph.open_graph ~options: spec.options base in
    match spec.default_graph with
       None -> g
     | Some file ->
        let size = g.Rdf_graph.size () in
        if size <= 0 then
          ignore(load_ttl g base file);
        g
  in
  let f (iri, file) =
    let g = Rdf_graph.open_graph base in
    load_ttl g base file ;
    (iri, g)
  in
  let named = List.map f spec.named in
  Rdf_ds.simple_dataset ~named default
;;

let print_solution solution =
  Rdf_sparql.solution_iter
    (fun name term -> print_string (name^"->"^(Rdf_term.string_of_term term)^" ; "))
    solution;
  print_newline()
;;

let print_result = function
  Error s -> print_endline ("ERROR: "^s)
| Ok (Solutions solutions) ->
   Printf.printf "%d Solution(s):\n" (List.length solutions);
   List.iter print_solution solutions
| Ok (Bool b) ->
   print_endline (if b then "true" else "false")
| Ok (Graph g) ->
    print_endline (Rdf_ttl.to_string g)
;;

(*********************************************************************************)
(*                OCaml-RDF                                                      *)
(*                                                                               *)
(*    Copyright (C) 2011 Institut National de Recherche en Informatique          *)
(*    et en Automatique. All rights reserved.                                    *)
(*                                                                               *)
(*    This program is free software; you can redistribute it and/or modify       *)
(*    it under the terms of the GNU Library General Public License version       *)
(*    2.1 or later as published by the Free Software Foundation.                 *)
(*                                                                               *)
(*    This program is distributed in the hope that it will be useful,            *)
(*    but WITHOUT ANY WARRANTY; without even the implied warranty of             *)
(*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the              *)
(*    GNU Library General Public License for more details.                       *)
(*                                                                               *)
(*    You should have received a copy of the GNU Library General Public          *)
(*    License along with this program; if not, write to the Free Software        *)
(*    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA                   *)
(*    02111-1307  USA                                                            *)
(*                                                                               *)
(*    Contact: Maxence.Guesdon@inria.fr                                          *)
(*                                                                               *)
(*                                                                               *)
(*********************************************************************************)

(* This example is a translation from the example1.c of the librdf
  distribution:
  https://github.com/dajobe/librdf/blob/master/examples/example1.c
*)

let usage = Printf.sprintf
  "USAGE: %s <RDF source URI> [rdf parser name]\n" Sys.argv.(0);;

let fatal msg = prerr_endline msg ; exit 1;;

let loop () =
  prerr_endline "loop";
  for i = 0 to 100000 do ignore(Unix.stat "/tmp")done;
  prerr_endline "end of loop"
;;

let main () =
  let len = Array.length Sys.argv in
  if len < 2 || len > 3 then fatal usage ;

  let world = Rdf_init.new_world () in
  Rdf_init.open_world world;
  let raptor_world = Rdf_raptor.new_world () in

  let uri = Rdf_uri.new_uri world Sys.argv.(1) in
  let storage = Rdf_storage.new_storage world ~factory: "memory" ~name: "test" in
  let model = Rdf_model.new_model world storage in

  let parser_name = if len > 2 then Some Sys.argv.(2) else None in
  let parser = Rdf_parser.new_parser ?name: parser_name world in

  (* PARSE the URI as RDF/XML *)
  print_endline
    (Printf.sprintf "%s: Parsing URI %s" Sys.argv.(0) (Rdf_uri.as_string uri));
  Rdf_parser.parse_into_model parser ~base: uri uri model;

  let () =
    let statement2 = Rdf_statement.new_from_nodes world
    (Rdf_node.new_from_uri_string world "http://www.dajobe.org/")
    (Rdf_node.new_from_uri_string world "http://purl.org/dc/elements/1.1/title")
    (Rdf_node.new_from_literal world "My home page")
    in
    Rdf_model.add_statement model statement2
  in

  (* Print out the model *)
  print_endline (Printf.sprintf "%s: Resulting model is:" Sys.argv.(0));
  let () =
    let iostream = Rdf_raptor.new_iostream_to_file_handle raptor_world Unix.stdout in
    Rdf_model.write model iostream
  in
  (*
     Construct the query predicate (arc) and object (target)
     and partial statement bits.
  *)
  let subject = Rdf_node.new_from_uri_string world "http://www.dajobe.org/" in
  let predicate = Rdf_node.new_from_uri_string world "http://purl.org/dc/elements/1.1/title" in
  let partial_statement = Rdf_statement.new_statement world in
  Rdf_statement.set_subject partial_statement subject;
  Rdf_statement.set_predicate partial_statement predicate;

  (* QUERY TEST 1 - use find_statements to match *)
  print_endline (Printf.sprintf "%s: Trying to find_statements" Sys.argv.(0));
  let stream = Rdf_model.find_statements model partial_statement in
  let rec iter count =
    if Rdf_stream.is_at_end stream then
      count
    else
      begin
        (
         match Rdf_stream.get_object stream with
           None ->
             Printf.printf "%s: stream_get_object returned None\n" Sys.argv.(0)
         | Some statement ->
             print_string "  Matched statement: ";
             flush stdout;
             Rdf_statement.print statement (Unix.stdout);
             print_endline ""
        );
        ignore(Rdf_stream.next stream);
        iter (count+1)
      end
  in
  let count = iter 0 in
  Printf.printf "%s: got %d matching statements\n" Sys.argv.(0) count;

  (* QUERY TEST 2 - use get_targets to do match *)
  print_endline (Printf.sprintf "%s: Trying to get targets" Sys.argv.(0));
  loop();
  let iterator = Rdf_model.get_targets model subject predicate in
  let rec iter count =
    if Rdf_iterator.is_at_end iterator then
      count
    else
      begin
        (
         match Rdf_iterator.get_object iterator Rdf_node.copy_node with
           None ->
             Printf.printf "%s: iterator_get_object returned None" Sys.argv.(0)
         | Some target ->
         Printf.printf "  Matched target: ";
         Rdf_node.print target Unix.stdout ;
         print_endline "";
        );
        ignore (Rdf_iterator.next iterator);
        iter (count + 1)
      end
  in
  let count = iter 0 in
  Printf.printf "%s: got %d target nodes\n" Sys.argv.(0) count;
  flush stdout;
  loop()
;;

let () = main ();;
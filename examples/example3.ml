(*********************************************************************************)
(*                OCaml-RDF                                                      *)
(*                                                                               *)
(*    Copyright (C) 2011 Institut National de Recherche en Informatique          *)
(*    et en Automatique. All rights reserved.                                    *)
(*                                                                               *)
(*    This program is free software; you can redistribute it and/or modify       *)
(*    it under the terms of the GNU Lesser General Public License version        *)
(*    2.1 or later as published by the Free Software Foundation.                 *)
(*                                                                               *)
(*    This program is distributed in the hope that it will be useful,            *)
(*    but WITHOUT ANY WARRANTY; without even the implied warranty of             *)
(*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the              *)
(*    GNU Library General Public License for more details.                       *)
(*                                                                               *)
(*    You should have received a copy of the GNU Lesser General Public           *)
(*    License along with this program; if not, write to the Free Software        *)
(*    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA                   *)
(*    02111-1307  USA                                                            *)
(*                                                                               *)
(*    Contact: Maxence.Guesdon@inria.fr                                          *)
(*                                                                               *)
(*                                                                               *)
(*********************************************************************************)

(* This example is a translation from the example3.c of the librdf
  distribution:
  https://github.com/dajobe/librdf/blob/master/examples/example3.c

  Redland example code creating model, statement and storing it in 10 lines.
*)

let main () =
  let world = Rdf_init.new_world () in
  Rdf_init.open_world world;
  let raptor_world = Rdf_raptor.new_world () in
  let model = Rdf_model.new_model world
    (Rdf_storage.new_storage world ~factory: "hashes" ~name: "test"
      ~options: "hash-type='bdb',dir='.'")
  in
  Rdf_model.add_statement model
  (Rdf_statement.new_from_nodes world
   (Rdf_node.new_from_uri_string world "http://www.dajobe.org/")
   (Rdf_node.new_from_uri_string world "http://purl.org/dc/elements/1.1/creator")
   (Rdf_node.new_from_literal world "Dave Beckette")
  );
  let iostr = Rdf_raptor.new_iostream_to_file_handle raptor_world Unix.stdout in
  Rdf_model.write model iostr
;;
let () = main ();;

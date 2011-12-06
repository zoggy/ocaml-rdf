(* This example is a translation from the example1.c of the librdf
  distribution:
  https://github.com/dajobe/librdf/blob/master/examples/example2.c

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

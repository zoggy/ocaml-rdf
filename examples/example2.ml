(* This example is a translation from the example1.c of the librdf
  distribution:
  https://github.com/dajobe/librdf/blob/master/examples/example2.c

 Redland example code parsing RDF/XML from a string in memory and adding/checking/removing a statement
*)


let rdfxml_content=
"<?xml version=\"1.0\"?>
<rdf:RDF xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\"
     xmlns:dc=\"http://purl.org/dc/elements/1.1/\">
  <rdf:Description rdf:about=\"http://www.dajobe.org/\">
    <dc:title>Dave Beckett's Home Page</dc:title>
    <dc:creator>Dave Beckett</dc:creator>
    <dc:description>The generic home page of Dave Beckett.</dc:description>
  </rdf:Description>
</rdf:RDF>
";;


let main () =
  let program = Sys.argv.(0) in
  let world = Rdf_init.new_world () in
  Rdf_init.open_world world;
  let raptor_world = Rdf_raptor.new_world () in
  let uri = Rdf_uri.new_uri world "http://example.librdf.org/" in

  let storage = Rdf_storage.new_storage world ~factory: "memory" ~name: "test" in
  let model = Rdf_model.new_model world storage in

  let parser = Rdf_parser.new_parser world ~name: "rdfxml" in
  Rdf_parser.parse_string_into_model parser rdfxml_content ~base: uri model;

  let statement = Rdf_statement.new_statement world in
  Rdf_statement.set_subject statement
    (Rdf_node.new_from_uri_string world "http://example.org/subject");
  Rdf_statement.set_predicate statement
    (Rdf_node.new_from_uri_string world "http://example.org/pred1");
  Rdf_statement.set_object statement
    (Rdf_node.new_from_literal world "object");

  Rdf_model.add_statement model statement;

  (* Print out the model *)
  print_endline (Printf.sprintf "%s: Resulting model is:" program);
  let () =
    let iostream = Rdf_raptor.new_iostream_to_file_handle raptor_world Unix.stdout in
    Rdf_model.write model iostream
  in
  (
    try
     if Rdf_model.contains_statement model statement then
       Printf.printf "%s: Model contains the statement\n" program;
   with
     Rdf_storage.Illegal_statement _ ->
       Printf.printf "Illegal statement, same as in original example\n"
  );

  Printf.printf "%s: Removing the statement\n" program;
  Rdf_model.remove_statement model statement;

  print_endline (Printf.sprintf "%s: Resulting model is:" program);
  let () =
    let iostream = Rdf_raptor.new_iostream_to_file_handle raptor_world Unix.stdout in
    Rdf_model.write model iostream
  in
  ()
;;
let () = main ();;
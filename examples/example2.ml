(* This example is a translation from the example1.c of the librdf
  distribution:
  https://github.com/dajobe/librdf/blob/master/examples/example2.c

 Redland example code parsing RDF/XML from a string in memory and adding/checking/removing a statement
*)


let rdfxml_content=
"<?xml version=\"1.0\"?>\
<rdf:RDF xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\"\
     xmlns:dc=\"http://purl.org/dc/elements/1.1/\">\
  <rdf:Description rdf:about=\"http://www.dajobe.org/\">\
    <dc:title>Dave Beckett's Home Page</dc:title>\
    <dc:creator>Dave Beckett</dc:creator>\
    <dc:description>The generic home page of Dave Beckett.</dc:description>\
  </rdf:Description> \
</rdf:RDF>\
";;

let main () =
  let program = Sys.argv.(0) in
(*int
main(int argc, char *argv[])
{
  librdf_world* world;
  librdf_storage* storage;
  librdf_model* model;
  librdf_parser* parser;
  librdf_statement* statement;
  librdf_uri* uri;
  char *program=argv[0];
  int rc=0;
  raptor_world *raptor_world_ptr;
  raptor_iostream* iostr;
  world=librdf_new_world();
  librdf_world_open(world);
  raptor_world_ptr = librdf_world_get_raptor(world);
*)
  let world = Rdf_init.new_world () in
  Rdf_init.open_world world;
  let raptor_world = Rdf_raptor.new_world () in
(*
  uri=librdf_new_uri(world, (const unsigned char* )"http://example.librdf.org/");
  if(!uri) {
    fprintf(stderr, "%s: Failed to create URI\n", program);
    return(1);
  }
*)
  let uri = Rdf_uri.new_uri world "http://example.librdf.org/" in
(*
  storage=librdf_new_storage(world, "memory", "test", NULL);
  if(!storage) {
    fprintf(stderr, "%s: Failed to create new storage\n", program);
    rc=1;
    goto tidyworld;
  }
*)
  let storage = Rdf_storage.new_storage world ~factory: "memory" ~name: "test" in
(*
  model=librdf_new_model(world, storage, NULL);
  if(!model) {
    fprintf(stderr, "%s: Failed to create model\n", program);
    rc=1;
    goto tidystorage;
  }
*)
  let model = Rdf_model.new_model world storage in
(*
  parser=librdf_new_parser(world, "rdfxml", NULL, NULL);
  if(!parser) {
    fprintf(stderr, "%s: Failed to create new parser 'rdfxml'\n", program);
    rc=1;
    goto tidystorage;
  }
*)
  let parser = Rdf_parser.new_parser world ~name: "rdfxml" in

(*
  if(librdf_parser_parse_string_into_model(parser, rdfxml_content, uri, model)) {
    fprintf(stderr, "%s: Failed to parse RDF into model\n", program);
    librdf_free_uri(uri);
    rc=1;
    goto tidymodel;
  }

  librdf_free_parser(parser); parser=NULL;
*)
   Rdf_parser.parse_string_into_model parser rdfxml_content ~base: uri model;

(*
  librdf_free_uri(uri); uri=NULL;


  statement=librdf_new_statement(world);
  if(!statement) {
    fprintf(stderr, "%s: Failed to parse RDF into model\n", program);
    rc=1;
    goto tidymodel;
  }

  librdf_statement_set_subject(statement,
                               librdf_new_node_from_uri_string(world, (const unsigned char* )"http://example.org/subject"));

  librdf_statement_set_predicate(statement,
                                   librdf_new_node_from_uri_string(world, (const unsigned char* )"http://example.org/pred1"));

  librdf_statement_set_object(statement,
                              librdf_new_node_from_literal(world, (const unsigned char* )"object", NULL, 0));

  librdf_model_add_statement(model, statement);
*)
  let statement = Rdf_statement.new_statement world in
  Rdf_statement.set_subject statement
    (Rdf_node.new_from_uri_string world "http://example.org/subject");
  Rdf_statement.set_predicate statement
    (Rdf_node.new_from_uri_string world "http://example.org/pred1");
  Rdf_statement.set_object statement
    (Rdf_node.new_from_literal world "object");
  Rdf_model.add_statement model statement;
(*
  fprintf(stdout, "%s: Resulting model is:\n", program);
  iostr = raptor_new_iostream_to_file_handle(raptor_world_ptr, stdout);
  librdf_model_write(model, iostr);
  raptor_free_iostream(iostr);
*)
  (* Print out the model *)
  print_endline (Printf.sprintf "%s: Resulting model is:" program);
  let () =
    let iostream = Rdf_raptor.new_iostream_to_file_handle raptor_world Unix.stdout in
    Rdf_model.write model iostream
  in
(*
  if(!librdf_model_contains_statement(model, statement)) {
    fprintf(stdout, "%s: Model does not contain statement\n", program);
    rc=1;
    goto tidystatement;
  } else
    fprintf(stdout, "%s: Model contains the statement\n", program);
*)
  if Rdf_model.model_contains_statement model statement then
    Printf.printf "%s: Model contains the statement\n", program;
(*
  fprintf(stdout, "%s: Removing the statement\n", program);
  librdf_model_remove_statement(model, statement);
*)
  Printf.printf "%s: Removing the statement\n" program;
  Rdf_model.remove_statement model statement;
(*
  fprintf(stdout, "%s: Resulting model is:\n", program);
  iostr = raptor_new_iostream_to_file_handle(raptor_world_ptr, stdout);
  librdf_model_write(model, iostr);
  raptor_free_iostream(iostr);
*)
  print_endline (Printf.sprintf "%s: Resulting model is:" program);
  let () =
    let iostream = Rdf_raptor.new_iostream_to_file_handle raptor_world Unix.stdout in
    Rdf_model.write model iostream
  in
  ()
;;
let () = main ();;
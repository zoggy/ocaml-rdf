
(*
int
main(int argc, char *argv[])
{
  librdf_world* world;
  librdf_storage* storage;
  librdf_parser* parser;
  librdf_model* model;
  librdf_stream* stream;
  librdf_node *subject, *predicate;
  librdf_iterator* iterator;
  librdf_statement *partial_statement, *statement2;
  char *program=argv[0];
  librdf_uri *uri;
  char *parser_name=NULL;
  int count;
  raptor_world *raptor_world_ptr;
  raptor_iostream* iostr;

  if(argc <2 || argc >3) {
    fprintf(stderr, "USAGE: %s: <RDF source URI> [rdf parser name]\n", program);
    return(1);
  }


  world = librdf_new_world();
  librdf_world_open(world);
  raptor_world_ptr = librdf_world_get_raptor(world);
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
  Rdf_init.world_open world;
  let raptor_world = Rdf_raptor.new_world () in

(*
  uri=librdf_new_uri(world, (const unsigned char* )argv[1]);
  if(!uri) {
    fprintf(stderr, "%s: Failed to create URI\n", program);
    return(1);
  }
*)
  let uri = Rdf_uri.new_uri world Sys.argv.(1) in
(*
  storage=librdf_new_storage(world, "memory", "test", NULL);
  if(!storage) {
    fprintf(stderr, "%s: Failed to create new storage\n", program);
    return(1);
  }
*)
  let storage = Rdf_storage.new_storage world ~factory: "memory" ~name: "test" in
(*
  model=librdf_new_model(world, storage, NULL);
  if(!model) {
    fprintf(stderr, "%s: Failed to create model\n", program);
    return(1);
  }
*)
  let model = Rdf_model.new_model world storage in
(*

  if(argc==3)
    parser_name=argv[2];

  parser=librdf_new_parser(world, parser_name, NULL, NULL);
  if(!parser) {
    fprintf(stderr, "%s: Failed to create new parser\n", program);
    return(1);
  }
*)
  let parser_name = if len > 2 then Some Sys.argv.(2) else None in
  let parser = Rdf_parser.new_parser ?name: parser_name world in

(*
  /* PARSE the URI as RDF/XML*/
  fprintf(stdout, "%s: Parsing URI %s\n", program, librdf_uri_as_string(uri));
  if(librdf_parser_parse_into_model(parser, uri, uri, model)) {
    fprintf(stderr, "%s: Failed to parse RDF into model\n", program);
    return(1);
  }
  librdf_free_parser(parser);
*)
  print_endline
    (Printf.sprintf "%s: Parsing URI %s" Sys.argv.(0) (Rdf_uri.uri_as_string uri));
  Rdf_parser.parser_parse_into_model parser ~base: uri uri model;

(*

  statement2=librdf_new_statement_from_nodes(world, librdf_new_node_from_uri_string(world, (const unsigned char* )"http://www.dajobe.org/"),
                                             librdf_new_node_from_uri_string(world, (const unsigned char* )"http://purl.org/dc/elements/1.1/title"),
                                             librdf_new_node_from_literal(world, (const unsigned char* )"My Home Page", NULL, 0)
                                             );
  librdf_model_add_statement(model, statement2);

  /* Free what we just used to add to the model - now it should be stored */
  librdf_free_statement(statement2);
*)
  let () =
    let statement2 = Rdf_statement.new_statement_from_nodes world
    (Rdf_node.new_node_from_uri_string world "http://www.dajobe.org/")
    (Rdf_node.new_node_from_uri_string world "http://purl.org/dc/elements/1.1/title")
    (Rdf_node.new_node_from_literal world "My home page")
    in
    Rdf_model.model_add_statement model statement2
  in

(*
  /* Print out the model*/
  fprintf(stdout, "%s: Resulting model is:\n", program);
  iostr = raptor_new_iostream_to_file_handle(raptor_world_ptr, stdout);
  librdf_model_write(model, iostr);
  raptor_free_iostream(iostr);
*)
  print_endline (Printf.sprintf "%s: Resulting model is:" Sys.argv.(0));
  let () =
    let iostream = Rdf_raptor.raptor_new_iostream_to_file_handle raptor_world stdout in
    Rdf_model.model_write model iostream
  in
(*

  /* Construct the query predicate (arc) and object (target)
   * and partial statement bits
   */
  subject=librdf_new_node_from_uri_string(world, (const unsigned char* )"http://www.dajobe.org/");
  predicate=librdf_new_node_from_uri_string(world, (const unsigned char* )"http://purl.org/dc/elements/1.1/title");
  if(!subject || !predicate) {
    fprintf(stderr, "%s: Failed to create nodes for searching\n", program);
    return(1);
  }
  partial_statement=librdf_new_statement(world);
  librdf_statement_set_subject(partial_statement, subject);
  librdf_statement_set_predicate(partial_statement, predicate);
*)
  let subject = Rdf_node.new_node_from_uri_string world "http://www.dajobe.org/" in
  let predicate = Rdf_node.new_node_from_uri_string world "http://purl.org/dc/elements/1.1/title" in
  let partial_statement = Rdf_statement.new_statement world in
  Rdf_statement.statement_set_subject partial_statement subject;
  Rdf_statement.statement_set_predicate partial_statement predicate;

(*

  /* QUERY TEST 1 - use find_statements to match */

  fprintf(stdout, "%s: Trying to find_statements\n", program);
  stream=librdf_model_find_statements(model, partial_statement);
  if(!stream) {
    fprintf(stderr, "%s: librdf_model_find_statements returned NULL stream\n", program);
  } else {
    count=0;
    while(!librdf_stream_end(stream)) {
      librdf_statement *statement=librdf_stream_get_object(stream);
      if(!statement) {
        fprintf(stderr, "%s: librdf_stream_next returned NULL\n", program);
        break;
      }

      fputs("  Matched statement: ", stdout);
      librdf_statement_print(statement, stdout);
      fputc('\n', stdout);

      librdf_stream_next(stream);
      count++;
    }
    librdf_free_stream(stream);
    fprintf(stderr, "%s: got %d matching statements\n", program, count);
  }
*)
  print_endline (Printf.sprintf "%s: Trying to find_statements" Sys.argv.(0));
  let stream = Rdf_model.model_find_statements model partial_statement in
  let rec iter count =
    if Rdf_stream.stream_end stream then
      count
    else
      begin
        (
         match Rdf_stream.stream_get_object stream with
           None ->
             Printf.printf "%s: stream_get_object returned None\n" Sys.argv.(0)
         | Some statement ->
             print_string "  Matched statement: ";
             flush stdout;
             Rdf_statement.statement_print statement stdout;
             print_endline ""
        );
        ignore(Rdf_stream.stream_next stream);
        iter (count+1)
      end
  in
  let count = iter 0 in
  Printf.printf "%s: got %d matching statements\n" Sys.argv.(0) count;
(*
  /* QUERY TEST 2 - use get_targets to do match */
  fprintf(stdout, "%s: Trying to get targets\n", program);
  iterator=librdf_model_get_targets(model, subject, predicate);
  if(!iterator)  {
    fprintf(stderr, "%s: librdf_model_get_targets failed to return iterator for searching\n", program);
    return(1);
  }
*)
  print_endline (Printf.sprintf "%s: Trying to get targets" Sys.argv.(0));
  let iterator = Rdf_model.model_get_targets model subject predicate in

(*
  count=0;
  while(!librdf_iterator_end(iterator)) {
    librdf_node *target;

    target=(librdf_node* )librdf_iterator_get_object(iterator);
    if(!target) {
      fprintf(stderr, "%s: librdf_iterator_get_object returned NULL\n", program);
      break;
    }

    fputs("  Matched target: ", stdout);
    librdf_node_print(target, stdout);
    fputc('\n', stdout);

    count++;
    librdf_iterator_next(iterator);
  }
  librdf_free_iterator(iterator);
  fprintf(stderr, "%s: got %d target nodes\n", program, count);
*)
  let rec iter count =
    if Rdf_iterator.iterator_end iterator then
      count
    else
      begin
        (
         match Rdf_iterator.iterator_get_object iterator Rdf_node.copy_node with
           None ->
             Printf.printf "%s: iterator_get_object returned None" Sys.argv.(0)
         | Some target ->
         Printf.printf "  Matched target: ";
         Rdf_node.node_print target stdout ;
         print_endline "";
        );
        ignore (Rdf_iterator.iterator_next iterator);
        iter (count + 1)
      end
  in
  let count = iter 0 in
  Printf.printf "%s: got %d target nodes\n" Sys.argv.(0) count;
  loop();
  let () =
    let statement2 = Rdf_statement.new_statement_from_nodes world
      (Rdf_node.new_node_from_uri_string world "http://www.dajobe.org/")
      (Rdf_node.new_node_from_uri_string world "http://purl.org/dc/elements/1.1/title")
      (Rdf_node.new_node_from_literal world "My home page2")
    in
    Rdf_model.model_add_statement model statement2
  in
  loop ()
(*
  librdf_free_statement(partial_statement);
  /* the above does this since they are still attached */
  /* librdf_free_node(predicate); */
  /* librdf_free_node(object); */

  librdf_free_model(model);

  librdf_free_storage(storage);

  librdf_free_uri(uri);

  librdf_free_world(world);

#ifdef LIBRDF_MEMORY_DEBUG
  librdf_memory_report(stderr);
#endif

  /* keep gcc -Wall happy */
  return(0);
}
*)

let () = main ();;
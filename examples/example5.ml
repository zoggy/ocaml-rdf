(* This example is a translation from the example5.c of the librdf
  distribution:
  https://github.com/dajobe/librdf/blob/master/examples/example5.c

   Redland example code using querying
*)

let usage = Printf.sprintf
  "USAGE: %s <CONTENT-URI> <QUERY-STRING>\n" Sys.argv.(0);;

let fatal msg = prerr_endline msg ; exit 1;;

let main () =
(*
  char *program=argv[0];
  librdf_world* world;
  librdf_storage *storage;
  librdf_model* model;
  const char *parser_name;
  librdf_parser* parser;
  librdf_query* query;
  librdf_query_results* results;
  librdf_uri *uri;
  const unsigned char *query_string=NULL;
  raptor_world *raptor_world_ptr;

  world=librdf_new_world();
  librdf_world_open(world);
  raptor_world_ptr = librdf_world_get_raptor(world);
*)
  let program = Sys.argv.(0) in
  let world = Rdf_init.new_world () in
  Rdf_init.open_world world;
  let raptor_world = Rdf_raptor.new_world () in
  Rdf_raptor.open_world raptor_world;
(*
  if(argc !=3) {
    fprintf(stderr, "USAGE: %s CONTENT-URI QUERY-STRING\n", program);
    return 1;
  }
*)
  if Array.length Sys.argv <> 3 then fatal usage;

(*
  uri=librdf_new_uri(world, (const unsigned char* )argv[1]);
  query_string=(const unsigned char* )argv[2];

  model=librdf_new_model(world, storage=librdf_new_storage(world, "hashes", "test", "new='yes',hash-type='bdb',dir='.'"), NULL);
  if(!model || !storage) {
    fprintf(stderr, "%s: Failed to make model or storage\n", program);
    return 1;
  }
*)
  let uri = Rdf_uri.new_uri world Sys.argv.(1) in
  let query_string = Sys.argv.(2) in
  let model = Rdf_model.new_model world
    (Rdf_storage.new_storage world ~factory: "hashes" ~name: "test"
      ~options: "new='yes',hash-type='bdb',dir='.'")
  in
  prerr_endline (Printf.sprintf "getting parser name with ident=%s" (Rdf_uri.as_string uri));
  let parser_name = Rdf_raptor.guess_parser_name
    ~ident: (Rdf_uri.as_string uri) raptor_world
  in

  prerr_endline "building parser";
  let parser = Rdf_parser.new_parser world ?name: parser_name in
  Rdf_parser.parse_into_model parser uri model;

  let query = Rdf_query.new_query ~name: "sparql" ~query: query_string world in
  let results = Rdf_model.query_execute model query in
  print_endline "Query execute OK";

  while not (Rdf_query_results.finished results) do
    match Rdf_query_results.get_bindings results with
      None -> prerr_endline "no bindings"
    | Some (names, nodes) ->
        print_string "results: ["; flush stdout;
        for i = 0 to Array.length names - 1 do
          Printf.printf "%s=" names.(i);
          flush stdout;
          (
           match nodes.(i) with
             None -> print_string ""
           | Some node -> Rdf_node.print node Unix.stdout
          );
          if i < Array.length names then print_string ", ";
        done;
        print_endline "]";
        ignore(Rdf_query_results.next results);
  done;
  Printf.printf "%s: Query returns %d results\n" program
    (Rdf_query_results.get_count results)
;;
let () = main ();;

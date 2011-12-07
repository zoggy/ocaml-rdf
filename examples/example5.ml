(* This example is a translation from the example5.c of the librdf
  distribution:
  https://github.com/dajobe/librdf/blob/master/examples/example5.c

   Redland example code using querying
*)

let usage = Printf.sprintf
  "USAGE: %s <CONTENT-URI> <QUERY-STRING>\n" Sys.argv.(0);;

let fatal msg = prerr_endline msg ; exit 1;;

let loop () =
  prerr_endline "loop";
  for i = 0 to 100000 do ignore(Unix.stat "/tmp")done;
  prerr_endline "end of loop"
;;

let main () =
  let program = Sys.argv.(0) in
  let world = Rdf_init.new_world () in
  Rdf_init.open_world world;
  let raptor_world = Rdf_raptor.new_world () in
  Rdf_raptor.open_world raptor_world;

  if Array.length Sys.argv <> 3 then fatal usage;

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
  loop();
(*
  begin
    match Rdf_query_results.to_string2 results with
      None -> prerr_endline "to_string2 failed"
    | Some s -> prerr_endline s
  end;
*)
(*
  Rdf_query_results.to_file_handle2 results Unix.stdout ;
*)
  let string_of_node = function
    None -> "NONE"
  | Some node ->
      match Rdf_node.get_literal_value node with
        None -> "Not a literal"
      | Some s -> s
  in
  prerr_endline "Entering while loop";
  while not (Rdf_query_results.finished results) do
    prerr_endline "Got a result";
    match Rdf_query_results.get_bindings results with
      None -> prerr_endline "no bindings"
    | Some (names, nodes) ->
        print_string "results: ["; flush stdout;
        let count =
          match Rdf_query_results.get_bindings_count results with
            None -> 0
          | Some n -> n
        in
        for i = 0 to count - 1 do
          let node_string = string_of_node nodes.(i) in
          let node_string2 = string_of_node (Rdf_query_results.get_binding_value results i) in
          Printf.printf "%s (%s)=%s (%s)"
           names.(i)
           (match Rdf_query_results.get_binding_name results i with None -> "NONE" | Some s -> s)
           node_string node_string2;
          if i < count - 1 then print_string ", ";
        done;
        print_endline "]";
        ignore(Rdf_query_results.next results);
  done;
  Printf.printf "%s: Query returns %d results\n" program
    (Rdf_query_results.get_count results);
  loop();
(*  ignore(Rdf_model.to_string model);*)
;;
let () = main ();;

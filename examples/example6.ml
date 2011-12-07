(* This example is a translation from the example6.c of the librdf
  distribution:
  https://github.com/dajobe/librdf/blob/master/examples/example6.c

   Redland example code using model methods load and to_string
*)

let loop () =
  prerr_endline "loop";
  for i = 0 to 100000 do ignore(Unix.stat "/tmp")done;
  prerr_endline "end of loop"
;;

let main () =
 let program = Sys.argv.(0) in
  let world = Rdf_init.new_world () in
  Rdf_init.open_world world;

  let model = Rdf_model.new_model world
    (Rdf_storage.new_storage world ~factory: "memory")
  in
(*
  uri=librdf_new_uri(world, (const unsigned char* )"http://planetrdf.com/index.rdf");
  librdf_model_load(model, uri, NULL, NULL, NULL);
*)
  let uri = Rdf_uri.new_uri world "http://planetrdf.com/index.rdf" in
  Rdf_model.load model uri;
(*
  string=librdf_model_to_string(model, uri, "ntriples", NULL, NULL);
  if(!string)
    printf("Failed to serialize model\n");
  else {
    printf("Made a %d byte string\n", (int)strlen((char* )string));
    free(string);
  }
*)
  begin
    match Rdf_model.to_string model ~uri ~name: "ntriples" with
      None -> failwith "Failed to serialize model"
    | Some string ->
        print_endline (Printf.sprintf "Make a %d byte string: %s"
         (String.length string) string);
  end;
  loop()
;;
let () = main ();;

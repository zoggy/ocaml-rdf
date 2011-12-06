(** Example *)


let loop () =
  prerr_endline "loop";
  for i = 0 to 100000 do ignore(Unix.stat "/tmp")done;
  prerr_endline "end of loop"
;;

let test_hash world =
  let hash = Rdf_hash.new_from_string world ~name: "memory" ~string: "y='1'" in
  Rdf_hash.put_strings hash ~key: "x" ~value: "2";
  match Rdf_hash.get hash "y" with
    None -> prerr_endline "NULL"
  | Some s -> prerr_endline (Printf.sprintf "y=%s" s)
;;

let x =
  let rasqal = Rdf_rasqal.new_world () in
  let _ =
    let raptor = Rdf_raptor.new_world () in
    prerr_endline (Printf.sprintf "raptor address: %s"
     (Nativeint.to_string (Rdf_raptor.Raw.pointer_of_world raptor)));
    Rdf_rasqal.world_set_raptor rasqal (Some raptor) ;
    loop();
    ignore(raptor);
    let world = Rdf_init.new_world () in
    Rdf_init.open_world world;
    Rdf_init.set_rasqal world (Some rasqal);
    Rdf_init.init_mutex world;
    Rdf_init.set_digest world "hello";
    test_hash world;
    let statement =
      Rdf_statement.new_from_nodes world
      (Rdf_node.new_from_uri_string world "http://www.dajobe.org/")
      (Rdf_node.new_from_uri_string world "http://purl.org/dc/elements/1.1/creator")
      (Rdf_node.new_from_literal world "Dave Beckett")
    in
    ()
  in
  let foo =
    let raptor = Rdf_rasqal.world_get_raptor rasqal in
    match raptor with
      None -> prerr_endline "No raptor !"
    | Some r ->
        prerr_endline
        (Printf.sprintf "OK: there is a raptor; address is %s"
         (Nativeint.to_string (Rdf_raptor.Raw.pointer_of_world r))
        );
  in
  ()
;;

loop();;
let () = ignore(x);;
let rasqal = ();;
loop();;
loop();;

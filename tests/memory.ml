(** Testing whether we live in harmony with the GC, with big loops. *)


let uri_s = "http://caml.inria/fr/hump";;

let nb_iterations = 500000;;

let test_new_node_from_uri_string world =
  prerr_endline "test_new_node_from_uri_string";
  for i = 0 to nb_iterations do
    let uri = Rdf_node.new_from_uri_string world uri_s in
    ignore(uri)
  done
;;

let tests = [
    test_new_node_from_uri_string ;
  ]
;;

let world = Rdf_init.new_world () ;;
let () = Rdf_init.open_world world;;

List.iter (fun f -> f world) tests;;
Gc.major();;
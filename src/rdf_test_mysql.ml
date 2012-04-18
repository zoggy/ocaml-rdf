(** *)

open Rdf_graph;;

let string_of_triple (sub, pred, obj) =
  Printf.sprintf "%s %s %s."
  (Rdf_types.string_of_node sub)
  (Rdf_types.string_of_node pred)
  (Rdf_types.string_of_node obj)
;;

let main () =
  let options =
    [ "storage", "mysql" ;
      "database", "genet";
      "user", "guesdon" ;
    ]
  in
  let g = Rdf_graph.open_graph ~options "http://hello.fr" in
  let pred = Rdf_types.node_of_uri_string "http://dis-bonjour.org" in
  let obj = Rdf_types.node_of_literal_string "youpi" in
  let sub = Rdf_types.node_of_uri_string "http://coucou0.net" in
  for i = 0 to 10 do
    g.add_triple
    ~sub: (Rdf_types.node_of_uri_string (Printf.sprintf "http://coucou%d.net" i))
    ~pred ~obj
  done;
  g.rem_triple
    ~sub: (Rdf_types.node_of_uri_string "http://coucou3.net")
    ~pred ~obj;
  let subjects = g.subjects_of ~pred ~obj in
  List.iter (fun node -> print_endline (Rdf_types.string_of_node node)) subjects;

  let b = g.exists_t (sub, pred, obj) in
  assert b;
  let b = g.exists ~sub ~obj () in
  assert b;
  let b = not (g.exists ~obj: (Rdf_types.node_of_uri_string "http://") ()) in
  assert b;
  let triples = g.find () in
  List.iter (fun t -> print_endline (string_of_triple t)) triples;

  let subjects = g.subjects () in
  List.iter (fun node -> print_endline (Rdf_types.string_of_node node)) subjects;

  let sub4 = Rdf_types.node_of_uri_string "http://coucou4.net" in
  g.transaction_start ();
  g.rem_triple ~sub: sub4 ~pred ~obj;
  assert (not (g.exists_t (sub4, pred, obj)));
  g.transaction_rollback ();
  assert (g.exists_t (sub4, pred, obj))
;;
let () = main();;

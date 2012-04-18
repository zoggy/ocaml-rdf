(** *)

open Rdf_graph;;

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
  for i = 0 to 10 do
    g.add_triple
    ~sub: (Rdf_types.node_of_uri_string (Printf.sprintf "http://coucou%d.net" i))
    ~pred ~obj
  done;
  g.rem_triple
    ~sub: (Rdf_types.node_of_uri_string "http://coucou3.net")
    ~pred ~obj;
  let subjects = g.subjects_of ~pred ~obj in
  List.iter (fun node -> prerr_endline (Rdf_types.string_of_node node)) subjects
;;
let () = main();;

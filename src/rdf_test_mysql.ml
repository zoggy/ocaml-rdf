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
  g.add_triple
   ~sub: (Rdf_types.node_of_uri_string "http://coucou.net")
    ~pred: (Rdf_types.node_of_uri_string "http://dis-bonjour.org")
    ~obj: (Rdf_types.node_of_literal_string "youpi");
  g.add_triple
   ~sub: (Rdf_types.node_of_uri_string "http://coucou2.net")
    ~pred: (Rdf_types.node_of_uri_string "http://dis-bonjour.org")
    ~obj: (Rdf_types.node_of_literal_string "youpi");
  g.rem_triple
   ~sub: (Rdf_types.node_of_uri_string "http://coucou.net")
    ~pred: (Rdf_types.node_of_uri_string "http://dis-bonjour.org")
    ~obj: (Rdf_types.node_of_literal_string "youpi");
;;
let () = main();;

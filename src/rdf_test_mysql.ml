(** *)

let graph =
  let options =
    [ "storage", "mysql" ;
      "database", "genet";
      "user", "guesdon" ;
    ]
  in
  Rdf_graph.open_graph ~options "http://hello.fr"
;;



let q = {sparql|PREFIX foaf:<http://xmlns.com/foaf/0.1/> SELECT ?name WHERE {}|sparql}
let q =
  try [%sparql
  "PREFIX foaf: %{term}
   SELECT ?name
   WHERE { ?x foaf:name ?name }"
      (Rdf_term.term_of_iri_string "http://xmlns.com/foaf/0.1/")
      ]
  with Rdf_sparql.Error e ->
    prerr_endline (Rdf_sparql.string_of_error e);
    exit 1

let base = Rdf_iri.iri "http://foo.bar"
let g = Rdf_graph.open_graph base
let () = Rdf_ttl.from_string g ~base
{|
@prefix foaf:        <http://xmlns.com/foaf/0.1/> .

_:a  foaf:name       "Alice" .
_:a  foaf:mbox       <mailto:alice@work.example> .

_:b  foaf:name       "Bob" .
_:b  foaf:mbox       <mailto:bob@work.example> .
|}

let eval_query query =
  let dataset = Rdf_ds.dataset g in
  match Rdf_sparql.execute ~base dataset query with
  | Rdf_sparql.Bool true -> print_endline "true"
  | Rdf_sparql.Bool false -> print_endline "false"
  | Rdf_sparql.Graph _ -> print_endline "graph"
  | Rdf_sparql.Solutions sols ->
      let f_sol sol =
        Rdf_sparql.solution_iter
          (fun name term -> print_string (name^"->"^(Rdf_term.string_of_term term)^" ; "))
          sol;
        print_newline()
      in
      print_endline "Solutions:";
      List.iter f_sol sols
;;

let () = eval_query q

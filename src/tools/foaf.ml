
let read_file base g file =
  try ignore(Rdf_ttl.from_file g base file)
  with Rdf_ttl.Error e ->
    prerr_endline (Rdf_ttl.string_of_error e)
;;
(*c==v=[File.file_of_string]=1.1====*)
let file_of_string ~file s =
  let oc = open_out file in
  output_string oc s;
  close_out oc
(*/c==v=[File.file_of_string]=1.1====*)

let main () =
  let base = Rdf_uri.uri "http://foo.net" in
  let g = Rdf_graph.open_graph base in
  Array.iter (read_file base g) (Array.sub Sys.argv 1 (Array.length Sys.argv - 1));
  let dot = Rdf_dot.dot_of_graph g in
  file_of_string ~file: "all.dot" dot;
  let q = Rdf_sparql.parse_from_string
    "PREFIX foaf: <http://xmlns.com/foaf/0.1/>
     SELECT DISTINCT ?name ?mbox
     WHERE { _:a foaf:name ?name .
             _:a foaf:mbox ?mbox .
           }"
  in
  let dataset = Rdf_ds.simple_dataset g in
  let sols = Rdf_sparql.select base dataset q in
  let f_sol sol =
    Rdf_sparql.solution_iter
      (fun v term -> print_string (v^"=>"^(Rdf_term.string_of_term term)^" "))
      sol;
    print_newline()
  in
  List.iter f_sol sols
;;

let () = main ()


(* TEST  *)

let print_header response =
  print_endline "";
  let status = Cohttp.Response.status response in
  print_endline ("status:" ^ (string_of_int (Cohttp.Code.code_of_status status)));
  let headers = Cohttp.Response.headers response in
  Cohttp.Header.iter (fun n c -> print_endline (n ^ ":" ^ (List.hd c))) headers

let print_solution s =
  let print n t = print_endline (Rdf_term.string_of_term t) in
  Rdf_sparql_ms.mu_iter print s

let print_response (header, body) =
  print_header header;
  List.iter (fun s -> print_endline ""; print_solution s) body

let main () =
  let base_url = "http://127.0.0.1:8000" in
  (* let graph = "http://example.com/graph3" in *)
  (* let graph = base_url in *)

  (* let update_query = "INSERT DATA { <http://pumgrana.com/content/detail/52780cbdc21477f7aa5b9107> <http://pumgrana.com/content/detail/52780d55c21477f7aa5b9108> \"Default\" }" *)
  (* in *)
  (* let delete_query = "DELETE DATA { GRAPH <"^graph^"> { <http://pumgrana.com/person/1234#person> <http://pumgrana.com/foaf/0.1/name> \"26\" } }" in *)
  (* lwt response = Rdf_4s.post_update base_url delete_query in *)
  (* print_response response; *)


  (* let append_data = "<http://pumgrana.com/person/1234#person> <http://pumgrana.com/foaf/0.1/name> \"26\" ." *)
  (* in *)
  (* lwt response = Rdf_4s.post_append base_url append_data graph in *)
  (* print_response response; *)


(*   let put_data = "<?xml version=\"1.0\" encoding=\"utf-8\" ?> *)
(* <rdf:RDF *)
(* xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\" *)
(* xmlns:rdfs=\"http://www.w3.org/2000/01/rdf-schema#\" *)
(* xmlns:owl=\"http://www.w3.org/2002/07/owl#\" *)
(* xmlns:dcterms=\"http://purl.org/dc/terms/\" *)
(* xmlns:ns4=\"http://dbpedia.org/ontology/Work/\" *)
(* xmlns:dbpedia-owl=\"http://dbpedia.org/ontology/\" *)
(* xmlns:foaf=\"http://xmlns.com/foaf/0.1/\" *)
(* xmlns:dbpprop=\"http://dbpedia.org/property/\" *)
(* xmlns:ns8=\"http://www.w3.org/ns/prov#\" > *)
(* <rdf:Description rdf:about=\"http://dbpedia.org/resource/Wintersun_(album)\"> *)
(* <rdf:type rdf:resource=\"http://www.w3.org/2002/07/owl#Thing\" /> *)
(* </rdf:Description> *)
(* </rdf:RDF>" *)
(*   in *)
(*   lwt response = Rdf_4s.put base_url put_data graph in *)
(*   print_response response; *)


  (* lwt response = Rdf_4s.delete base_url graph in *)
  (* print_response response; *)


  let get_query = "SELECT ?s ?p ?o WHERE { ?s ?p ?o }" in
  (* let get_query = "SELECT ?g WHERE { GRAPH ?g { <http://pumgrana.com/content/detail/52780cbdc21477f7aa5b9107> <http://pumgrana.com/content/detail/52780d55c21477f7aa5b9108> \"Test\" } }" in *)
  lwt response = Rdf_4s.get base_url get_query in
  print_response response;

  Lwt.return ()

let _ = Lwt_main.run (main ())

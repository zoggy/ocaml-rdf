(*
 ocamlfind ocamlopt -package rdf.lwt -linkpkg -o t.x ask.ml
*)

let query_dbpedia =
  "PREFIX owl: <http://www.w3.org/2002/07/owl#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX foaf: <http://xmlns.com/foaf/0.1/>
PREFIX dc: <http://purl.org/dc/elements/1.1/>
PREFIX : <http://dbpedia.org/resource/>
PREFIX dbpedia2: <http://dbpedia.org/property/>
PREFIX dbpedia: <http://dbpedia.org/>
PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
PREFIX dbpedia-owl: <http://dbpedia.org/ontology/>
PREFIX dbpprop: <http://dbpedia.org/property/>

  ASK
   { ?city dbpedia-owl:inseeCode ?codeinsee .
           ?city dbpprop:name ?ville .
           ?city dbpedia-owl:populationTotal ?population .
           ?city dbpprop:populationDate ?date .
           ?city rdf:type dbpedia-owl:Settlement .
           ?city dbpedia-owl:country :France .
           FILTER ( (?population > 10000) &&
                    (?date >= \"2008\"^^<http://www.w3.org/2001/XMLSchema#integer>)
                  )
         }
   "

let (>>=) = Lwt.bind;;

open Rdf_sparql_protocol;;

let query uri q =
  let mes = {
      in_query = q ;
      in_dataset = Rdf_sparql_protocol.empty_dataset ;
    }
  in
  let iri = Rdf_iri.of_uri uri in
  let graph = Rdf_graph.open_graph iri in
  Rdf_sparql_http_lwt.get ~graph ~base: iri uri mes
    ~accept: "application/sparql-results+xml"
    >>=
    function
      Ok -> Lwt_io.write Lwt_io.stdout "OK"
    | Error e -> failwith (Rdf_sparql_protocol.string_of_error e)
    | Result r ->
        match r with
          Rdf_sparql.Solutions l ->
            Lwt_io.write Lwt_io.stdout (Printf.sprintf "%d solutions returned" (List.length l))
        | Rdf_sparql.Bool b ->
            Lwt_io.write Lwt_io.stdout (if b then "true" else "false")
        | _ -> assert false
;;

Lwt_main.run
 (query (Rdf_uri.uri "http://dbpedia.org/sparql") query_dbpedia);;
 

(** RDF/JSON *)

exception Unexpected_json of string * Yojson.Basic.json (** context * json *)

val term_of_json : Yojson.Basic.json -> Rdf_term.term

val sparql_result : Yojson.Basic.json -> Rdf_sparql.query_result

(** *)

type in_dataset = {
  inds_default : Rdf_iri.iri option;
  inds_named : Rdf_iri.iri list ;
  }

let empty_dataset = { inds_default = None ; inds_named = [] }

type in_message = {
    in_query : string ;
    in_dataset : in_dataset ;
  }

type error =
  | Malformed_query of string
  | Query_request_refused of string
  | Error_other of string

type out_message =
| Ok
| Result of Rdf_sparql.query_result
| Error of error

let string_of_error = function
| Malformed_query s -> "Malformed query: "^s
| Query_request_refused s -> "Query request refused: "^s
| Error_other s -> s



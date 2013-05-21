(** *)

open Rdf_ttl_types;;

let from_string g ~base s =
  let ctx = { base = base ; prefixes = Rdf_ttl_types.SMap.empty } in
  let lexbuf = Ulexing.from_utf8_string s in
  let parse = Rdf_ulex.menhir_with_ulex Rdf_ttl_parser.main Rdf_ttl_lex.main in
  let statements =
    try parse lexbuf
    with Rdf_ttl_parser.Error -> failwith "Parse error"
  in
  ()
  
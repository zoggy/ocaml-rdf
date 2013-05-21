(** *)

open Rdf_ttl_types;;

exception Parse_error of int * int * string

let from_lexbuf g ~base lexbuf =
  let ctx = { base = base ; prefixes = Rdf_ttl_types.SMap.empty } in
  let parse = Rdf_ulex.menhir_with_ulex Rdf_ttl_parser.main Rdf_ttl_lex.main in
  let statements =
    try parse lexbuf
    with Rdf_ttl_parser.Error ->
        let (start, stop) = Ulexing.loc lexbuf in
        let lexeme = Ulexing.utf8_lexeme lexbuf in
        raise (Parse_error (start, stop, lexeme))
  in
  ()

let from_string g ~base s =
  let lexbuf = Ulexing.from_utf8_string s in
  from_lexbuf g ~base lexbuf
;;

let from_file g ~base file =
  let ic = open_in file in
  let lexbuf = Ulexing.from_utf8_channel ic in
  try from_lexbuf g ~base lexbuf
  with e ->
      close_in ic;
      raise e
;;
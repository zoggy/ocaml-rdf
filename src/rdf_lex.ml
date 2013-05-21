(** *)

open Rdf_parser;;

let regexp iriref = '<' ([^ '<' '>' '"' '{' '}' '|' '^' '`' '\\' ' ' 0000-0020])* '>'

let main = lexer
  iriref -> assert false
| _ -> assert false
;;
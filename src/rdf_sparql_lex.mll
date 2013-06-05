(** *)

open Rdf_sparql_types;;
open Rdf_sparql_parser;;

let mk_loc lb =
  let start =
    { Lexing.pos_lnum = 0 ; pos_bol = 0 ; pos_cnum = Ulexing.lexeme_start lb; pos_fname = "" }
  in
  let stop =
    { Lexing.pos_lnum = 0 ; pos_bol = 0 ; pos_cnum = Ulexing.lexeme_end lb ; pos_fname = "" }
  in
  { Rdf_sparql_types.loc_start = start ; loc_end = stop }
;;

let utf8_find_char t c =
  let len = Array.length t in
  let rec iter p =
    if p >= len then raise Not_found ;
    if t.(p) = c then p else iter (p+1)
  in
  iter 0
;;
let regexp hex = ['0'-'9'] | ['A'-'F'] | ['a'-'f']
let regexp codepoint_u = "\\u" hex hex hex hex
let regexp codepoint_U = "\\U" hex hex hex hex hex hex hex hex
let regexp codepoint_any = [0x00-0x10FFFF]

let regexp iriref = '<' ([^ '<' '>' '"' '{' '}' '|' '^' '`' '\\' ' ' 0000-0020])* '>'

let regexp pn_chars_base = ['A'-'Z'] | ['a'-'z'] | [0x00C0-0x00D6] | [0x00D8-0x00F6] | [0x00F8-0x02FF] | [0x0370-0x037D] | [0x037F-0x1FFF] | [0x200C-0x200D] | [0x2070-0x218F] | [0x2C00-0x2FEF] | [0x3001-0xD7FF] | [0xF900-0xFDCF] | [0xFDF0-0xFFFD] | [0x10000-0xEFFFF]
let regexp pn_chars_u = pn_chars_base | '_'
let regexp pn_chars = pn_chars_u | '-' | ['0'-'9'] | 0x00B7 | [0x0300-0x036F] | [0x203F-0x2040]
let regexp pn_prefix = pn_chars_base ((pn_chars|'.')* pn_chars)?
let regexp pname_ns = pn_prefix? ':'
let regexp pn_local_esc = '\\' ( '_' | '~' | '.' | '-' | '!' | '$' | '&' | "'" | '(' | ')' | '*' | '+' | ',' | ';' | '=' | '/' | '?' | '#' | '@' | '%' )

let regexp percent = '%' hex hex
let regexp plx = percent | pn_local_esc
let regexp pn_local = (pn_chars_u | ':' | ['0'-'9'] | plx ) ((pn_chars | '.' | ':' | plx)* (pn_chars | ':' | plx) )?
let regexp pname_ln = pname_ns pn_local
let regexp blank_node_label = "_:" ( pn_chars_u | ['0'-'9'] ) ((pn_chars|'.')* pn_chars)?
let regexp varname = ( pn_chars_u | ['0'-'9'] ) ( pn_chars_u | ['0'-'9'] | 0x00B7 | [0x0300-0x036F] | [0x203F-0x2040] )*
let regexp var1 = '?' varname
let regexp var2 = '$' varname
let regexp langtag = '@' ['a'-'z''A'-'Z']+ ('-' ['a'-'z''A'-'Z''0'-'9']+)*
let regexp integer = ['0'-'9']+
let regexp decimal = ['0'-'9']* '.' ['0'-'9']+
let regexp exponent = ['e''E'] ['+''-']? ['0'-'9']+
let regexp double = ['0'-'9']+ '.' ['0'-'9']* exponent | '.' (['0'-'9'])+ exponent | (['0'-'9'])+ exponent
let regexp integer_positive = '+'integer
let regexp decimal_positive = '+'decimal
let regexp double_positive = '+'double
let regexp integer_negative = '-'integer
let regexp decimal_negative = '-'decimal
let regexp double_negative = '-'double
let regexp boolean = "true" | "false"
let regexp echar = '\\' ['t' 'b' 'n' 'r' 'f' '"' '\'']
let regexp string_literal1 = "'" ( ([^0x27 0x5C 0xA 0xD]) | echar )* "'"
let regexp string_literal2 = '"' ( ([^0x22 0x5C 0xA 0xD]) | echar )* '"'
let regexp string_literal_long1 = "'''" ( ( "'" | "''" )? ( [^'\'' '\\'] | echar ) )* "'''"
let regexp string_literal_long2 = "\"\"\"" ( ( '"' | "\"\"" )? ( [^'"' '\\'] | echar ) )* "\"\"\""
let regexp ws = 0x20 | 0x9 | 0xD | 0xA
let regexp nil = '(' ws* ')'
let regexp anon = '[' ws* ']';;


let rec main = lexer
| pname_ns ->
  let s = Ulexing.utf8_lexeme lexbuf in
  let t =
    { Rdf_sparql_types.pname_ns_loc = mk_loc lexbuf ;
      pname_ns_name = String.sub s 0 (String.length s - 1) ; (* remove final ':' *)
    }
  in
  Pname_ns t

| '*' -> STAR
| ':' -> COLON
| '(' -> LPAR
| ')' -> RPAR
| '{' -> LBRACE
| '}' -> RBRACE
| ',' -> COMMA
| '.' -> DOT
| 'a' -> A
| '|' -> PIPE
| "||" -> PIPEPIPE
| '/' -> SLASH
| '^' -> HAT
| '!' -> BANG
| '?' -> QM
| '+' -> PLUS
| '[' -> LBRACKET
| ']' -> RBRACKET
| ';' -> SEMICOLON
| "&&" -> AMPAMP

| '(' ws* ')' -> NIL

| ws -> main lexbuf

| integer -> Integer (Ulexing.utf8_lexeme lexbuf)
| decimal -> Decimal (Ulexing.utf8_lexeme lexbuf)
| double -> Double (Ulexing.utf8_lexeme lexbuf)
| integer_positive -> Integer_positive (Ulexing.utf8_lexeme lexbuf)
| decimal_positive -> Decimal_positive (Ulexing.utf8_lexeme lexbuf)
| double_positive -> Double_positive (Ulexing.utf8_lexeme lexbuf)
| integer_negative -> Integer_negative (Ulexing.utf8_lexeme lexbuf)
| decimal_negative -> Decimal_negative (Ulexing.utf8_lexeme lexbuf)
| double_negative -> Double_negative (Ulexing.utf8_lexeme lexbuf)

| boolean -> Boolean (Ulexing.utf8_lexeme lexbuf)

| ('a'|'A') ('s'|'S')  -> AS
| ('a'|'A') ('s'|'S') ('c'|'C')  -> ASC
| ('a'|'A') ('s'|'S') ('k'|'K')  -> ASK
| ('b'|'B') ('a'|'A') ('s'|'S') ('e'|'E')  -> BASE
| ('b'|'B') ('i'|'I') ('n'|'N') ('d'|'D')  -> BIND
| ('b'|'B') ('y'|'Y')  -> BY
| ('c'|'C') ('o'|'O') ('n'|'N') ('s'|'S') ('t'|'T') ('r'|'R') ('u'|'U') ('c'|'C') ('t'|'T')  -> CONSTRUCT
| ('d'|'D') ('e'|'E') ('s'|'S') ('c'|'C')  -> DESC
| ('d'|'D') ('e'|'E') ('s'|'S') ('c'|'C') ('r'|'R') ('i'|'I') ('b'|'B') ('e'|'E')  -> DESCRIBE
| ('d'|'D') ('i'|'I') ('s'|'S') ('t'|'T') ('i'|'I') ('n'|'N') ('c'|'C') ('t'|'T')  -> DISTINCT
| ('f'|'F') ('i'|'I') ('l'|'L') ('t'|'T') ('e'|'E') ('r'|'R')  -> FILTER
| ('f'|'F') ('r'|'R') ('o'|'O') ('m'|'M')  -> FROM
| ('g'|'G') ('r'|'R') ('a'|'A') ('p'|'P') ('h'|'H')  -> GRAPH
| ('g'|'G') ('r'|'R') ('o'|'O') ('u'|'U') ('p'|'P')  -> GROUP
| ('h'|'H') ('a'|'A') ('v'|'V') ('i'|'I') ('n'|'N') ('g'|'G')  -> HAVING
| ('l'|'L') ('i'|'I') ('m'|'M') ('i'|'I') ('t'|'T')  -> LIMIT
| ('m'|'M') ('i'|'I') ('n'|'N') ('u'|'U') ('s'|'S')  -> MINUS
| ('n'|'N') ('a'|'A') ('m'|'M') ('e'|'E') ('d'|'D')  -> NAMED
| ('o'|'O') ('f'|'F') ('f'|'F') ('s'|'S') ('e'|'E') ('t'|'T')  -> OFFSET
| ('o'|'O') ('p'|'P') ('t'|'T') ('i'|'I') ('o'|'O') ('n'|'N') ('a'|'A') ('l'|'L')  -> OPTIONAL
| ('o'|'O') ('r'|'R') ('d'|'D') ('e'|'E') ('r'|'R')  -> ORDER
| ('p'|'P') ('r'|'R') ('e'|'E') ('f'|'F') ('i'|'I') ('x'|'X')  -> PREFIX
| ('r'|'R') ('e'|'E') ('d'|'D') ('u'|'U') ('c'|'C') ('e'|'E') ('d'|'D')  -> REDUCED
| ('s'|'S') ('e'|'E') ('l'|'L') ('e'|'E') ('c'|'C') ('t'|'T')  -> SELECT
| ('s'|'S') ('i'|'I') ('l'|'L') ('e'|'E') ('n'|'N') ('t'|'T')  -> SILENT
| ('s'|'S') ('e'|'E') ('r'|'R') ('v'|'V') ('i'|'I') ('c'|'C') ('e'|'E')  -> SERVICE
| ('u'|'U') ('n'|'N') ('d'|'D') ('e'|'E') ('f'|'F')  -> UNDEF
| ('u'|'U') ('n'|'N') ('i'|'I') ('o'|'O') ('n'|'N')  -> UNION
| ('v'|'V') ('a'|'A') ('l'|'L') ('u'|'U') ('e'|'E') ('s'|'S')  -> VALUES
| ('w'|'W') ('h'|'H') ('e'|'E') ('r'|'R') ('e'|'E')  -> WHERE

| var1 ->
  let s = Ulexing.utf8_lexeme lexbuf in
  let len = String.length s in
  (* remove beginning ? *)
  let label = String.sub s 1 (len - 1) in
  Var1 label

| var2 ->
  let s = Ulexing.utf8_lexeme lexbuf in
  let len = String.length s in
  (* remove beginning $ *)
  let label = String.sub s 1 (len - 1) in
  Var2 label

| blank_node_label ->
  let s = Ulexing.utf8_lexeme lexbuf in
  let len = String.length s in
  (* remove beginning _: *)
  let label = String.sub s 2 (len - 2) in
  Blank_node_label label

| anon ->
  ANON

| pname_ln ->
  let t = Ulexing.lexeme lexbuf in
  let p =
    try utf8_find_char t (Char.code ':')
    with Not_found -> assert false
  in
  let ns = Ulexing.utf8_sub_lexeme lexbuf 0 p in
  let ln =
    let len = Array.length t in
    if len > p + 1 then
      Ulexing.utf8_sub_lexeme lexbuf (p+1) ((len - p) - 1)
    else
      ""
  in
  let loc = mk_loc lexbuf in
  let pl =
    match ln with
      "" -> None
    | _ ->
        Some { pname_local_loc = loc ; pname_local_name = ln }
  in
  Pname_ln
    {
      pname_loc = loc ;
      pname_ns = { pname_ns_loc = loc ; pname_ns_name = ns } ;
      pname_local = pl ;
    }

| iriref ->
  let s = Ulexing.utf8_lexeme lexbuf in
  let s = String.sub s 1 (String.length s - 2) in
  let loc = mk_loc lexbuf in
  Iriref_ { ir_loc = loc ; ir_iri = Rdf_uri.uri s }

| eof ->
    EOF
| _ ->
  failwith (Printf.sprintf "Unexpected lexeme: %S" (Ulexing.utf8_lexeme lexbuf));
;;

let int_of_hex c =
  match c with
  | '0'..'9' -> (Char.code c) - 48
  | 'a'..'f' -> (Char.code c) - 87
  | 'A'..'F' -> (Char.code c) - 55
  | _ -> failwith (Printf.sprintf "Invalid hex character %C" c)
;;

let int_of_hexcp s =
  let rec iter f acc p =
    if p < 0 then
      acc
    else
      (
       let acc = acc + (int_of_hex s.[p]) * f in
       iter (f * 16) acc (p-1)
      )
  in
  iter 1 0 (String.length s - 1)
;;

let rec codepoint b = lexer
| codepoint_u
| codepoint_U ->
    let lexeme = Ulexing.utf8_lexeme lexbuf in
    (* remove \u or \U from the beginning of the lexeme *)
    let hex = String.sub lexeme 2 (String.length lexeme - 2) in
    let cp = int_of_hexcp hex in
    Buffer.add_string b (Rdf_utf8.utf8_char_of_code cp);
    codepoint b lexbuf
| codepoint_any ->
    let lexeme = Ulexing.utf8_lexeme lexbuf in
    Buffer.add_string b lexeme;
    codepoint b lexbuf
| eof -> Buffer.contents b
;;

let unescape_codepoints s =
  let lexbuf = Ulexing.from_utf8_string s in
  let b = Buffer.create 256 in
  codepoint b lexbuf
;;
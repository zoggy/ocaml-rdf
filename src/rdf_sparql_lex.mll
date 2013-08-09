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

let add_echar b = function
  | 'b' -> Buffer.add_char b '\b'
(*  | 'f' -> Buffer.add_char b '\f'*)
  | 'n' -> Buffer.add_char b '\n'
  | 'r' -> Buffer.add_char b '\r'
  | 't' -> Buffer.add_char b '\t'
  | '\\' -> Buffer.add_char b '\\'
  | '"' -> Buffer.add_char b '"'
  | '\'' -> Buffer.add_char b '\''
  | c -> Buffer.add_char b c
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
let regexp echar = ['t' 'b' 'n' 'r' 'f' '\\' '"' '\'']

let regexp schar_quote = ([^0x27 0x5C 0xA 0xD])+
let regexp schar_dquote = ([^0x22 0x5C 0xA 0xD])+

(*
let regexp string_literal1 = "'" ( ([^0x27 0x5C 0xA 0xD]) | echar )* "'"
let regexp string_literal2 = '"' ( ([^0x22 0x5C 0xA 0xD]) | echar )* '"'
let regexp string_literal_long1 = "'''" ( ( "'" | "''" )? ( [^'\'' '\\'] | echar ) )* "'''"
let regexp string_literal_long2 = "\"\"\"" ( ( '"' | "\"\"" )? ( [^'"' '\\'] | echar ) )* "\"\"\""
*)

let regexp longstring_quote_delim = 0x27 0x27 0x27
let regexp longstring_dquote_delim = 0x22 0x22 0x22

let regexp ws = 0x20 | 0x9 | 0xD | 0xA
let regexp nil = '(' ws* ')'
let regexp anon = '[' ws* ']';;


let rec string_quote b line = lexer
| '\'' -> line, String_literal (Buffer.contents b)
| '\\' echar ->
  let s = Ulexing.utf8_lexeme lexbuf in
  (* unescape some characters *)
  add_echar b s.[0];
  string_quote b line lexbuf
| schar_quote ->
    let s = Ulexing.utf8_lexeme lexbuf in
    Buffer.add_string b s ;
    string_quote b line lexbuf
| eof ->
   failwith "Unterminated quoted string"
;;

let rec string_dquote b line = lexer
| '"' ->
  let s = Buffer.contents b in
  (line + Rdf_utf8.utf8_count_nl s, String_literal s)
| '\\' echar ->
  let s = Ulexing.utf8_lexeme lexbuf in
  (* unescape some characters *)
  add_echar b s.[0];
  string_dquote b line lexbuf
| schar_dquote ->
    let s = Ulexing.utf8_lexeme lexbuf in
    Buffer.add_string b s ;
    string_dquote b line lexbuf
| eof ->
   failwith "Unterminated double quoted string"
;;


let rec longstring_quote b line = lexer
| longstring_quote_delim ->
    let s = Buffer.contents b in
    (line + Rdf_utf8.utf8_count_nl s, String_literal s)
| '\\' echar ->
  let s = Ulexing.utf8_lexeme lexbuf in
  (* unescape some characters *)
  add_echar b s.[0];
  longstring_quote b line lexbuf
| ('\'' | ( [^'\'' '\\'] | '\n')* ) ->
    let s = Ulexing.utf8_lexeme lexbuf in
    Buffer.add_string b s ;
    longstring_quote b line lexbuf
| eof ->
   failwith "Unterminated long quoted string"
;;


let rec longstring_dquote b line = lexer
| longstring_dquote_delim ->
  let s = Buffer.contents b in
  (line + Rdf_utf8.utf8_count_nl s, String_literal s)
| '\\' echar ->
  let s = Ulexing.utf8_lexeme lexbuf in
  (* unescape some characters *)
  add_echar b s.[0];
  longstring_dquote b line lexbuf
| ('"' | ( [^'"' '\\'] | '\n')* ) ->
    let s = Ulexing.utf8_lexeme lexbuf in
    Buffer.add_string b s ;
    longstring_dquote b line lexbuf
| eof ->
   failwith "Unterminated long double-quoted string"
;;

let rec main line = lexer
| pname_ns ->
  let s = Ulexing.utf8_lexeme lexbuf in
  let t =
    { Rdf_sparql_types.pname_ns_loc = mk_loc lexbuf ;
      pname_ns_name = String.sub s 0 (String.length s - 1) ; (* remove final ':' *)
    }
  in
  line, Pname_ns t

| '*' -> line, STAR
| '(' -> line, LPAR
| ')' -> line, RPAR
| '{' -> line, LBRACE
| '}' -> line, RBRACE
| ',' -> line, COMMA
| '.' -> line, DOT
| 'a' -> line, A
| '|' -> line, PIPE
| "||" -> line, PIPEPIPE
| '/' -> line, SLASH
| '^' -> line, HAT
| "^^" -> line, HATHAT
| '!' -> line, BANG
| '?' -> line, QM
| '+' -> line, PLUS
| '[' -> line, LBRACKET
| ']' -> line, RBRACKET
| ';' -> line, SEMICOLON
| "&&" -> line, AMPAMP
| '=' -> line, EQUAL
| "!=" -> line, NOTEQUAL
| '<' -> line, LT
| '>' -> line, GT
| "<=" -> line, LTE
| ">=" -> line, GTE

| '(' ws* ')' ->
   let s = Ulexing.utf8_lexeme lexbuf in
   (line + Rdf_utf8.utf8_count_nl s, NIL)

| ws ->
  let s = Ulexing.utf8_lexeme lexbuf in
  main (line + Rdf_utf8.utf8_count_nl s) lexbuf

| langtag ->
  let s = Ulexing.utf8_lexeme lexbuf in
  let len = String.length s in
  line, Langtag (String.sub s 1 (len - 1))

| longstring_quote_delim ->
    (*prerr_endline "entering long quoted string";*)
    longstring_quote (Buffer.create 256) line lexbuf
| longstring_dquote_delim ->
    (*prerr_endline "entering long dquoted string";*)
    longstring_dquote (Buffer.create 256) line lexbuf
| '\'' ->
    (*prerr_endline "entering quoted string";*)
    string_quote (Buffer.create 256) line lexbuf
| '"' ->
    (*prerr_endline "entering dquoted string";*)
    string_dquote (Buffer.create 256) line lexbuf
(*
| string_literal1 ->
  let s = Ulexing.utf8_lexeme lexbuf in
  let len = String.length s in
  String_literal1 (String.sub s 1 (len - 2))

| string_literal2 ->
  let s = Ulexing.utf8_lexeme lexbuf in
  let len = String.length s in
  String_literal2 (String.sub s 1 (len - 2))

| string_literal_long1 ->
  let s = Ulexing.utf8_lexeme lexbuf in
  let len = String.length s in
  String_literal1 (String.sub s 3 (len - 6))

| string_literal_long2 ->
  let s = Ulexing.utf8_lexeme lexbuf in
  let len = String.length s in
  String_literal1 (String.sub s 3 (len - 6))
*)

| integer -> line, Integer (Ulexing.utf8_lexeme lexbuf)
| decimal -> line, Decimal (Ulexing.utf8_lexeme lexbuf)
| double -> line, Double (Ulexing.utf8_lexeme lexbuf)
| integer_positive -> line, Integer_positive (Ulexing.utf8_lexeme lexbuf)
| decimal_positive -> line, Decimal_positive (Ulexing.utf8_lexeme lexbuf)
| double_positive -> line, Double_positive (Ulexing.utf8_lexeme lexbuf)
| integer_negative -> line, Integer_negative (Ulexing.utf8_lexeme lexbuf)
| decimal_negative -> line, Decimal_negative (Ulexing.utf8_lexeme lexbuf)
| double_negative -> line, Double_negative (Ulexing.utf8_lexeme lexbuf)

| boolean -> line, Boolean (Ulexing.utf8_lexeme lexbuf)

| ('a'|'A') ('b'|'B') ('s'|'S')  -> line, ABS
| ('a'|'A') ('s'|'S')  -> line, AS
| ('a'|'A') ('s'|'S') ('c'|'C')  -> line, ASC
| ('a'|'A') ('s'|'S') ('k'|'K')  -> line, ASK
| ('a'|'A') ('v'|'V') ('g'|'G')  -> line, AVG
| ('b'|'B') ('a'|'A') ('s'|'S') ('e'|'E')  -> line, BASE
| ('b'|'B') ('i'|'I') ('n'|'N') ('d'|'D')  -> line, BIND

(* support for old BINDINGS keywords, replaced by VALUES
 see change log at the end of http://www.w3.org/TR/sparql11-federated-query/
*)
| ('b'|'B') ('i'|'I') ('n'|'N') ('d'|'D') ('i'|'I') ('n'|'N') ('g'|'G') ('s'|'S')  -> line, VALUES

| ('b'|'B') ('n'|'N') ('o'|'O') ('d'|'D') ('e'|'E')  -> line, BNODE
| ('b'|'B') ('n'|'N') ('o'|'O') ('d'|'D') ('e'|'E')  -> line, BNODE
| ('b'|'B') ('o'|'O') ('u'|'U') ('n'|'N') ('d'|'D')  -> line, BOUND
| ('b'|'B') ('y'|'Y')  -> line, BY
| ('c'|'C') ('e'|'E') ('i'|'I') ('l'|'L')  -> line, CEIL
| ('c'|'C') ('o'|'O') ('a'|'A') ('l'|'L') ('e'|'E') ('s'|'S') ('c'|'C') ('e'|'E')  -> line, COALESCE
| ('c'|'C') ('o'|'O') ('n'|'N') ('c'|'C') ('a'|'A') ('t'|'T')  -> line, CONCAT
| ('c'|'C') ('o'|'O') ('n'|'N') ('s'|'S') ('t'|'T') ('r'|'R') ('u'|'U') ('c'|'C') ('t'|'T')  -> line, CONSTRUCT
| ('c'|'C') ('o'|'O') ('n'|'N') ('t'|'T') ('a'|'A') ('i'|'I') ('n'|'N') ('s'|'S')  -> line, CONTAINS
| ('c'|'C') ('o'|'O') ('u'|'U') ('n'|'N') ('t'|'T')  -> line, COUNT
| ('d'|'D') ('a'|'A') ('t'|'T') ('a'|'A') ('t'|'T') ('y'|'Y') ('p'|'P') ('e'|'E')  -> line, DATATYPE
| ('d'|'D') ('a'|'A') ('y'|'Y')  -> line, DAY
| ('d'|'D') ('e'|'E') ('s'|'S') ('c'|'C')  -> line, DESC
| ('d'|'D') ('e'|'E') ('s'|'S') ('c'|'C') ('r'|'R') ('i'|'I') ('b'|'B') ('e'|'E')  -> line, DESCRIBE
| ('d'|'D') ('i'|'I') ('s'|'S') ('t'|'T') ('i'|'I') ('n'|'N') ('c'|'C') ('t'|'T')  -> line, DISTINCT
| ('e'|'E') ('n'|'N') ('c'|'C') ('o'|'O') ('d'|'D') ('e'|'E') ('_'|'_') ('f'|'F') ('o'|'O') ('r'|'R') ('_'|'_') ('u'|'U') ('r'|'R') ('i'|'I')  -> line, ENCODE_FOR_URI
| ('e'|'E') ('x'|'X') ('i'|'I') ('s'|'S') ('t'|'T') ('s'|'S')  -> line, EXISTS
| ('f'|'F') ('i'|'I') ('l'|'L') ('t'|'T') ('e'|'E') ('r'|'R')  -> line, FILTER
| ('f'|'F') ('l'|'L') ('o'|'O') ('o'|'O') ('r'|'R')  -> line, FLOOR
| ('f'|'F') ('r'|'R') ('o'|'O') ('m'|'M')  -> line, FROM
| ('g'|'G') ('r'|'R') ('a'|'A') ('p'|'P') ('h'|'H')  -> line, GRAPH
| ('g'|'G') ('r'|'R') ('o'|'O') ('u'|'U') ('p'|'P')  -> line, GROUP
| ('g'|'G') ('r'|'R') ('o'|'O') ('u'|'U') ('p'|'P') ('_'|'_') ('c'|'C') ('o'|'O') ('n'|'N') ('c'|'C') ('a'|'A') ('t'|'T')  -> line, GROUP_CONCAT
| ('h'|'H') ('a'|'A') ('v'|'V') ('i'|'I') ('n'|'N') ('g'|'G')  -> line, HAVING
| ('h'|'H') ('o'|'O') ('u'|'U') ('r'|'R') ('s'|'S')  -> line, HOURS
| ('i'|'I') ('f'|'F')  -> line, IF
| ('i'|'I') ('n'|'N') -> line, IN
| ('i'|'I') ('r'|'R') ('i'|'I')  -> line, IRI
| ('i'|'I') ('s'|'S') ('b'|'B') ('l'|'L') ('a'|'A') ('n'|'N') ('k'|'K')  -> line, ISBLANK
| ('i'|'I') ('s'|'S') ('i'|'I') ('r'|'R') ('i'|'I')  -> line, ISIRI
| ('i'|'I') ('s'|'S') ('l'|'L') ('i'|'I') ('t'|'T') ('e'|'E') ('r'|'R') ('a'|'A') ('l'|'L')  -> line, ISLITERAL
| ('i'|'I') ('s'|'S') ('n'|'N') ('u'|'U') ('m'|'M') ('e'|'E') ('r'|'R') ('i'|'I') ('c'|'C')  -> line, ISNUMERIC
| ('i'|'I') ('s'|'S') ('u'|'U') ('r'|'R') ('i'|'I')  -> line, ISURI
| ('l'|'L') ('a'|'A') ('n'|'N') ('g'|'G')  -> line, LANG
| ('l'|'L') ('a'|'A') ('n'|'N') ('g'|'G') ('m'|'M') ('a'|'A') ('t'|'T') ('c'|'C') ('h'|'H') ('e'|'E') ('s'|'S')  -> line, LANGMATCHES
| ('l'|'L') ('c'|'C') ('a'|'A') ('s'|'S') ('e'|'E')  -> line, LCASE
| ('l'|'L') ('i'|'I') ('m'|'M') ('i'|'I') ('t'|'T')  -> line, LIMIT
| ('m'|'M') ('a'|'A') ('x'|'X')  -> line, MAX
| ('m'|'M') ('d'|'D') ('5'|'5')  -> line, MD5
| ('m'|'M') ('i'|'I') ('n'|'N')  -> line, MIN
| ('m'|'M') ('i'|'I') ('n'|'N') ('u'|'U') ('s'|'S')  -> line, MINUS
| ('m'|'M') ('i'|'I') ('n'|'N') ('u'|'U') ('t'|'T') ('e'|'E') ('s'|'S')  -> line, MINUTES
| ('m'|'M') ('o'|'O') ('n'|'N') ('t'|'T') ('h'|'H')  -> line, MONTH
| ('n'|'N') ('a'|'A') ('m'|'M') ('e'|'E') ('d'|'D')  -> line, NAMED
| ('n'|'N') ('o'|'O') ('t'|'T') -> line, NOT
| ('n'|'N') ('o'|'O') ('w'|'W')  -> line, NOW
| ('o'|'O') ('f'|'F') ('f'|'F') ('s'|'S') ('e'|'E') ('t'|'T')  -> line, OFFSET
| ('o'|'O') ('p'|'P') ('t'|'T') ('i'|'I') ('o'|'O') ('n'|'N') ('a'|'A') ('l'|'L')  -> line, OPTIONAL
| ('o'|'O') ('r'|'R') ('d'|'D') ('e'|'E') ('r'|'R')  -> line, ORDER
| ('p'|'P') ('r'|'R') ('e'|'E') ('f'|'F') ('i'|'I') ('x'|'X')  -> line, PREFIX
| ('r'|'R') ('a'|'A') ('n'|'N') ('d'|'D')  -> line, RAND
| ('r'|'R') ('e'|'E') ('d'|'D') ('u'|'U') ('c'|'C') ('e'|'E') ('d'|'D')  -> line, REDUCED
| ('r'|'R') ('e'|'E') ('g'|'G') ('e'|'E') ('x'|'X') -> line, REGEX
| ('r'|'R') ('e'|'E') ('p'|'P') ('l'|'L') ('a'|'A') ('c'|'C') ('e'|'E')  -> line, REPLACE
| ('r'|'R') ('o'|'O') ('u'|'U') ('n'|'N') ('d'|'D')  -> line, ROUND
| ('s'|'S') ('a'|'A') ('m'|'M') ('e'|'E') ('t'|'T') ('e'|'E') ('r'|'R') ('m'|'M')  -> line, SAMETERM
| ('s'|'S') ('a'|'A') ('m'|'M') ('p'|'P') ('l'|'L') ('e'|'E')  -> line, SAMPLE
| ('s'|'S') ('e'|'E') ('c'|'C') ('o'|'O') ('n'|'N') ('d'|'D') ('s'|'S')  -> line, SECONDS
| ('s'|'S') ('e'|'E') ('l'|'L') ('e'|'E') ('c'|'C') ('t'|'T')  -> line, SELECT
| ('s'|'S') ('e'|'E') ('p'|'P') ('a'|'A') ('r'|'R') ('a'|'A') ('t'|'T') ('o'|'O') ('r'|'R')  -> line, SEPARATOR
| ('s'|'S') ('e'|'E') ('r'|'R') ('v'|'V') ('i'|'I') ('c'|'C') ('e'|'E')  -> line, SERVICE
| ('s'|'S') ('h'|'H') ('a'|'A') ('1'|'1')  -> line, SHA1
| ('s'|'S') ('h'|'H') ('a'|'A') ('2'|'2') ('5'|'5') ('6'|'6')  -> line, SHA256
| ('s'|'S') ('h'|'H') ('a'|'A') ('3'|'3') ('8'|'8') ('4'|'4')  -> line, SHA384
| ('s'|'S') ('h'|'H') ('a'|'A') ('5'|'5') ('1'|'1') ('2'|'2')  -> line, SHA512
| ('s'|'S') ('i'|'I') ('l'|'L') ('e'|'E') ('n'|'N') ('t'|'T')  -> line, SILENT
| ('s'|'S') ('t'|'T') ('r'|'R')  -> line, STR
| ('s'|'S') ('t'|'T') ('r'|'R') ('a'|'A') ('f'|'F') ('t'|'T') ('e'|'E') ('r'|'R')  -> line, STRAFTER
| ('s'|'S') ('t'|'T') ('r'|'R') ('b'|'B') ('e'|'E') ('f'|'F') ('o'|'O') ('r'|'R') ('e'|'E')  -> line, STRBEFORE
| ('s'|'S') ('t'|'T') ('r'|'R') ('d'|'D') ('t'|'T')  -> line, STRDT
| ('s'|'S') ('t'|'T') ('r'|'R') ('e'|'E') ('n'|'N') ('d'|'D') ('s'|'S')  -> line, STRENDS
| ('s'|'S') ('t'|'T') ('r'|'R') ('l'|'L') ('a'|'A') ('n'|'N') ('g'|'G')  -> line, STRLANG
| ('s'|'S') ('t'|'T') ('r'|'R') ('l'|'L') ('e'|'E') ('n'|'N')  -> line, STRLEN
| ('s'|'S') ('t'|'T') ('r'|'R') ('s'|'S') ('t'|'T') ('a'|'A') ('r'|'R') ('t'|'T') ('s'|'S')  -> line, STRSTARTS
| ('s'|'S') ('t'|'T') ('r'|'R') ('u'|'U') ('u'|'U') ('i'|'I') ('d'|'D')  -> line, STRUUID
| ('s'|'S') ('u'|'U') ('b'|'B') ('s'|'S') ('t'|'T') ('r'|'R')  -> line, SUBSTR
| ('s'|'S') ('u'|'U') ('m'|'M')  -> line, SUM
| ('t'|'T') ('i'|'I') ('m'|'M') ('e'|'E') ('z'|'Z') ('o'|'O') ('n'|'N') ('e'|'E')  -> line, TIMEZONE
| ('t'|'T') ('z'|'Z')  -> line, TZ
| ('u'|'U') ('c'|'C') ('a'|'A') ('s'|'S') ('e'|'E')  -> line, UCASE
| ('u'|'U') ('n'|'N') ('d'|'D') ('e'|'E') ('f'|'F')  -> line, UNDEF
| ('u'|'U') ('n'|'N') ('i'|'I') ('o'|'O') ('n'|'N')  -> line, UNION
| ('u'|'U') ('r'|'R') ('i'|'I')  -> line, URI
| ('u'|'U') ('u'|'U') ('i'|'I') ('d'|'D')  -> line, UUID
| ('v'|'V') ('a'|'A') ('l'|'L') ('u'|'U') ('e'|'E') ('s'|'S')  -> line, VALUES
| ('w'|'W') ('h'|'H') ('e'|'E') ('r'|'R') ('e'|'E')  -> line, WHERE
| ('y'|'Y') ('e'|'E') ('a'|'A') ('r'|'R')  -> line, YEAR

| var1 ->
  let s = Ulexing.utf8_lexeme lexbuf in
  let len = String.length s in
  (* remove beginning ? *)
  let label = String.sub s 1 (len - 1) in
  line, Var1 label

| var2 ->
  let s = Ulexing.utf8_lexeme lexbuf in
  let len = String.length s in
  (* remove beginning $ *)
  let label = String.sub s 1 (len - 1) in
  line, Var2 label

| blank_node_label ->
  let s = Ulexing.utf8_lexeme lexbuf in
  let len = String.length s in
  (* remove beginning _: *)
  let label = String.sub s 2 (len - 2) in
  line, Blank_node_label label

| anon ->
  line, ANON

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
  let s = Ulexing.utf8_lexeme lexbuf in
  (line + Rdf_utf8.utf8_count_nl s,
   Pname_ln
    {
      pname_loc = loc ;
      pname_ns = { pname_ns_loc = loc ; pname_ns_name = ns } ;
      pname_local = pl ;
    }
  )

| iriref ->
  let s = Ulexing.utf8_lexeme lexbuf in
  let s = String.sub s 1 (String.length s - 2) in
  let loc = mk_loc lexbuf in
  line, Iriref_ { ir_loc = loc ; ir_iri = Rdf_uri.uri s }

| eof ->
    line, EOF
| _ ->
  let msg = Printf.sprintf "Unexpected lexeme: %S"
    (Ulexing.utf8_lexeme lexbuf)
  in
  failwith msg
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
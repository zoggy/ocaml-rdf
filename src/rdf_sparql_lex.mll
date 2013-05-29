(** *)

open Rdf_sparql_parser;;

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
let regexp echar = '\\' ['t' 'b' 'n' 'r' 'f' '"' '\'']
let regexp string_literal1 = "'" ( ([^0x27 0x5C 0xA 0xD]) | echar )* "'"
let regexp string_literal2 = '"' ( ([^0x22 0x5C 0xA 0xD]) | echar )* '"'
let regexp string_literal_long1 = "'''" ( ( "'" | "''" )? ( [^'\'' '\\'] | echar ) )* "'''"
let regexp string_literal_long2 = "\"\"\"" ( ( '"' | "\"\"" )? ( [^'"' '\\'] | echar ) )* "\"\"\""
let regexp ws = 0x20 | 0x9 | 0xD | 0xA
let regexp nil = '(' ws* ')'
let regexp anon = '[' ws* ']'

let main = lexer
  iriref ->
      let s = Ulexing.utf8_lexeme lexbuf in
      Iriref (String.sub s 1 (String.length s - 2))
| _ -> assert false
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
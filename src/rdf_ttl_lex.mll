(** *)

open Rdf_ttl_parser;;

let regexp hex = [0x30-0x39] | [0x41-0x46]


let regexp character = "\\u" hex hex hex hex | "\\U" hex hex hex hex hex hex hex hex | '\\' | [0x20-0x5B] | [0x5D-0x10FFFF]
let regexp character_noquotes = "\\u" hex hex hex hex | "\\U" hex hex hex hex hex hex hex hex | '\\' character | [0x20-0x21] | [0x23-0x5B] | [0x5D-0x10FFFF]
let regexp character_nogt = "\\u" hex hex hex hex | "\\U" hex hex hex hex hex hex hex hex | '\\' | [0x20-0x3D] | [0x3F-0x5B] | [0x5D-0x10FFFF]

(*let regexp echaracter = character | '\t' | '\n' | '\r'*)
let regexp echaracter_noquotes = character_noquotes | '\t' | '\n' | '\r'

let regexp scharacter = echaracter_noquotes | "\\\""
let regexp longstring_delim = 0x22 0x22 0x22
let regexp string = 0x22 scharacter* 0x22

let regexp ucharacter = ( character_nogt) | "\\>"
let regexp relativeURI = ucharacter*
let regexp uriref = '<' relativeURI '>'

let regexp langtag = ['a'-'z']+ ('-' ['a'-'z''0'-'9']+)*

let regexp nameStartChar_nounderscore = ['A'-'Z'] | ['a'-'z'] | [0x00C0-0x00D6] | [0x00D8-0x00F6] | [0x00F8-0x02FF] | [0x0370-0x037D] | [0x037F-0x1FFF] | [0x200C-0x200D] | [0x2070-0x218F] | [0x2C00-0x2FEF] | [0x3001-0xD7FF] | [0xF900-0xFDCF] | [0xFDF0-0xFFFD] | [0x10000-0xEFFFF]

let regexp nameStartChar = nameStartChar_nounderscore | '_'
let regexp nameChar = nameStartChar | '-' | ['0'-'9'] | 0x00B7 | [0x0300-0x036F] | [0x203F-0x2040]

let regexp prefixName = ( nameStartChar_nounderscore ) nameChar*

let regexp name = nameStartChar nameChar*
let regexp qname = prefixName? ':' name?

let regexp nodeid = "_:" name

let regexp comment = '#' ( [^0xA 0xD] )*
let regexp ws = 0x20 | 0x9 | 0xD | 0xA

let regexp integer = ('-' | '+') ? ['0'-'9']+
let regexp exponent = ['e' 'E'] ('-' | '+')? ['0'-'9']+
let regexp double = ('-' | '+') ? ( ['0'-'9']+ '.' ['0'-'9']* exponent | '.' (['0'-'9'])+ exponent | (['0'-'9'])+ exponent )
let regexp decimal = ('-' | '+')? ( ['0'-'9']+ '.' ['0'-'9']* | '.' (['0'-'9'])+ | (['0'-'9'])+ )
let regexp boolean = "true" | "false"

let rec longstring b = lexer
| longstring_delim ->
    String_ (Buffer.contents b)
| '\\' character ->
    let s = Ulexing.utf8_lexeme lexbuf in
    (* unescape some characters *)
    (match s.[1] with
       'n' -> Buffer.add_char b '\n'
     | 'r' -> Buffer.add_char b '\r'
     | 't' -> Buffer.add_char b '\t'
     | '\\' -> Buffer.add_char b '\\'
     | '"' -> Buffer.add_char b '"'
     | _ -> Buffer.add_string b s
    );
    longstring b lexbuf
| ('"' | ( [^'"' '\\'] | '\n')* ) ->
    let s = Ulexing.utf8_lexeme lexbuf in
    Buffer.add_string b s ;
    longstring b lexbuf
| eof ->
   failwith "Unterminated long string"
;;

let rec main = lexer
| 'a' -> A
| "\r\n" | '\r' | '\n' -> main lexbuf
| comment -> main lexbuf
| ws -> main lexbuf
| '(' -> LEFT_PAR
| ')' -> RIGHT_PAR
| "[]" -> EMPTY_BRACKETS
| '[' -> LEFT_BRACKET
| ']' -> RIGHT_BRACKET
| ',' -> COMMA
| ';' -> SEMICOLON
| '.' -> DOT
| "@prefix" -> AT_PREFIX
| "@base" -> AT_BASE
| '@' -> AT
| "^^" -> HATHAT
| boolean ->
      let s = Ulexing.utf8_lexeme lexbuf in
      Boolean s
| integer ->
      let s = Ulexing.utf8_lexeme lexbuf in
      Integer s
| decimal ->
      let s = Ulexing.utf8_lexeme lexbuf in
      Decimal s
| double ->
      let s = Ulexing.utf8_lexeme lexbuf in
      Double s
| uriref ->
      let s = Ulexing.utf8_lexeme lexbuf in
      Uriref_ (String.sub s 1 (String.length s - 2))
| prefixName? ':' ->
      let s = Ulexing.utf8_lexeme lexbuf in
      (*prerr_endline (Printf.sprintf "prefixName %s" s);*)
      Identifier (String.sub s 0 (String.length s - 1))
| nodeid ->
  let s = Ulexing.utf8_lexeme lexbuf in
  let id = String.sub s 2 (String.length s - 2) in
  Bname id
| qname ->
  let s = Ulexing.utf8_lexeme lexbuf in
  let p = String.index s ':' in
  let s1 =
    match String.sub s 0 p with
      "" -> None
    | s -> Some s
  in
  let s2 =
    let len = String.length s in
    if len > p + 1 then
      Some (String.sub s (p+1) (len - p - 1))
    else
      None
  in
  Qname_ (s1, s2)

| langtag ->
      let s = Ulexing.utf8_lexeme lexbuf in
      Identifier s
| longstring_delim -> longstring (Buffer.create 256) lexbuf
| string ->
   let s = Ulexing.utf8_lexeme lexbuf in
   String_ (String.sub s 1 (String.length s - 2))
| eof -> EOF
| _ ->
  let s = Ulexing.utf8_lexeme lexbuf in
  failwith (Printf.sprintf "Lexeme %S not handled" s)
;;



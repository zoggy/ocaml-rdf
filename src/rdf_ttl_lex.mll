(*********************************************************************************)
(*                OCaml-RDF                                                      *)
(*                                                                               *)
(*    Copyright (C) 2012-2014 Institut National de Recherche en Informatique     *)
(*    et en Automatique. All rights reserved.                                    *)
(*                                                                               *)
(*    This program is free software; you can redistribute it and/or modify       *)
(*    it under the terms of the GNU Lesser General Public License version        *)
(*    3 as published by the Free Software Foundation.                            *)
(*                                                                               *)
(*    This program is distributed in the hope that it will be useful,            *)
(*    but WITHOUT ANY WARRANTY; without even the implied warranty of             *)
(*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the              *)
(*    GNU General Public License for more details.                               *)
(*                                                                               *)
(*    You should have received a copy of the GNU General Public License          *)
(*    along with this program; if not, write to the Free Software                *)
(*    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA                   *)
(*    02111-1307  USA                                                            *)
(*                                                                               *)
(*    Contact: Maxence.Guesdon@inria.fr                                          *)
(*                                                                               *)
(*********************************************************************************)

(** *)

open Rdf_ttl_parser;;

let reserved_chars =
  List.fold_right Rdf_types.CSet.add
    ['~' ; '.' ; '-' ; '!' ; '$' ; '&' ; '\'' ;
     '(' ; ')'; '*'; '+'; ',' ; ';' ; '=' ; '/' ;
     '?' ; '#' ; '@'; '%' ; '_' ]
    Rdf_types.CSet.empty;;

let unescape_reserved_chars =
  let rec iter b len s i =
   if i >= len then
      ()
    else
      begin
        let size = Rdf_utf8.utf8_nb_bytes_of_char s.[i] in
        let next =
          match size, s.[i] with
            1, '\\' ->
              if i + size < len &&
                Rdf_types.CSet.mem s.[i+size] reserved_chars
              then
                (
                 Buffer.add_char b s.[i+size];
                 i+size+1
                )
              else
                (
                 Buffer.add_char b s.[i] ;
                 i + size
                )
          | _ ->
              Buffer.add_string b (String.sub s i size);
              i + size
        in
        iter b len s next
      end
 in
 fun s ->
   let len = String.length s in
    let b = Buffer.create len in
    iter b len s 0 ;
    Buffer.contents b
;;



let regexp hex = ['0'-'9'] | ['A'-'F'] | ['a'-'f']
let regexp pn_chars_base = ['A'-'Z'] | ['a'-'z'] | [0x00C0-0x00D6] | [0x00D8-0x00F6] | [0x00F8-0x02FF] | [0x0370-0x037D] | [0x037F-0x1FFF] | [0x200C-0x200D] | [0x2070-0x218F] | [0x2C00-0x2FEF] | [0x3001-0xD7FF] | [0xF900-0xFDCF] | [0xFDF0-0xFFFD] | [0x10000-0xEFFFF]
let regexp pn_chars_u = pn_chars_base | '_'
let regexp pn_chars = pn_chars_u | '-' | ['0'-'9'] | 0x00B7 | [0x0300-0x036F] | [0x203F-0x2040]
let regexp pn_prefix = pn_chars_base ((pn_chars | '.')* pn_chars)?
let regexp percent = '%' hex hex
let regexp pn_local_sec = '\\' ('_' | '~' | '.' | '-' | '!' | '$' | '&' | "'" | '(' | ')' | '*' | '+' | ',' | ';' | '=' | '/' | '?' | '#' | '@' | '%')
let regexp plx = percent | pn_local_sec
let regexp pn_local = (pn_chars_u | ':' | ['0'-'9'] | plx) ((pn_chars | '.' | ':' | plx)* (pn_chars | ':' | plx))?

let regexp uchar =	"\\u" hex hex hex hex | "\\U" hex hex hex hex hex hex hex hex
let regexp iriref = '<' ([^0x00-0x20'<' '>' '\\' '"' '{' '}' '|' '^' '`'] | uchar)* '>'
let regexp pname_ns = pn_prefix? ':'
let regexp pname_ln = pname_ns pn_local
let regexp blank_node_label = "_:" (pn_chars_u | ['0'-'9']) ((pn_chars | '.')* pn_chars)?
let regexp langtag = '@' ['a'-'z' 'A'-'Z']+ ('-' ['a'-'z' 'A'-'Z' '0'-'9']+)*
let regexp integer = ['+' '-']? ['0'-'9']+
let regexp decimal = ['+' '-']? ['0'-'9']* '.' ['0'-'9']+
let regexp exponent = ['e' 'E'] ['+' '-']? ['0' - '9']+
let regexp double = ['+' '-']? (['0'-'9']+ '.' ['0'-'9']* exponent | '.' ['0'-'9']+ exponent | ['0'-'9']+ exponent)
let regexp echar = '\\' ['t' 'b' 'n' 'r' 'f' '\\' '"' '\\']
let regexp string_literal_quote = '"' ([^0x22 0x5C 0xA 0xD] | echar | uchar)* '"'
let regexp string_literal_single_quote = "'" ([^ 0x27 0x5C 0xA 0xD] | echar | uchar)* "'"
let regexp string_literal_long_single_quote = "'''" (("'" | "''")? [^'\'' '\\'] | echar | uchar)* "'''"
let regexp string_literal_long_quote = "\"\"\"" (('"' | "\"\"")? [^'"' '\\'] | echar | uchar)* "\"\"\""
let regexp ws = 0x20 | 0x9 | 0xD | 0xA
let regexp anon = '[' ws* ']'
let regexp comment = '#' ( [^0xA 0xD] )*
let regexp boolean = "true" | "false"

let regexp sparql_base = ('b'|'B') ('a'|'A') ('s'|'S') ('e'|'E')
let regexp sparql_prefix = ('p'|'P') ('r'|'R') ('e'|'E') ('f'|'F') ('i'|'I') ('x'|'X')

let lexpos = Rdf_ulex.lexpos
let lexpos_nl = Rdf_ulex.lexpos_nl


let rec main pos = lexer
| 'a' -> lexpos pos lexbuf, A
| "\r\n" -> main (lexpos_nl pos lexbuf) lexbuf
| '\r' -> main (lexpos pos lexbuf) lexbuf
| '\n' -> main (lexpos_nl pos lexbuf) lexbuf
| comment -> main (lexpos pos lexbuf) lexbuf
| ws -> main (lexpos_nl pos lexbuf) lexbuf
| anon -> (lexpos_nl pos lexbuf), ANON
| '(' -> (lexpos pos lexbuf), LEFT_PAR
| ')' -> (lexpos pos lexbuf), RIGHT_PAR
| '[' -> (lexpos pos lexbuf), LEFT_BRACKET
| ']' -> (lexpos pos lexbuf), RIGHT_BRACKET
| ',' -> (lexpos pos lexbuf), COMMA
| ';' -> (lexpos pos lexbuf), SEMICOLON
| '.' -> (lexpos pos lexbuf), DOT
| "@prefix" -> (lexpos pos lexbuf), AT_PREFIX
| "@base" -> (lexpos pos lexbuf), AT_BASE
| sparql_prefix -> (lexpos pos lexbuf), PREFIX
| sparql_base -> (lexpos pos lexbuf), BASE
| langtag ->
      let s = Ulexing.utf8_lexeme lexbuf in
      (lexpos pos lexbuf), At_identifier s
| "^^" -> (lexpos pos lexbuf), HATHAT
| boolean ->
      let s = Ulexing.utf8_lexeme lexbuf in
      (lexpos pos lexbuf), Boolean s
| integer ->
      let s = Ulexing.utf8_lexeme lexbuf in
      (lexpos pos lexbuf), Integer s
| decimal ->
      let s = Ulexing.utf8_lexeme lexbuf in
      (lexpos pos lexbuf), Decimal s
| double ->
      let s = Ulexing.utf8_lexeme lexbuf in
      (lexpos pos lexbuf), Double s
| iriref ->
      let s = Ulexing.utf8_lexeme lexbuf in
      let iri = String.sub s 1 (String.length s - 2) in
      let iri = Rdf_utf8.utf8_unescape iri in
      (lexpos pos lexbuf), Iriref_ iri
| pname_ns ->
      let s = Ulexing.utf8_lexeme lexbuf in
      (*prerr_endline (Printf.sprintf "pname_ns %s" s);*)
      (lexpos pos lexbuf), Identifier (String.sub s 0 (String.length s - 1))
| blank_node_label ->
  let s = Ulexing.utf8_lexeme lexbuf in
  let id = String.sub s 2 (String.length s - 2) in
  (lexpos pos lexbuf), Bname id
| pname_ln ->
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
      Some (unescape_reserved_chars (String.sub s (p+1) (len - p - 1)))
    else
      None
  in
  (lexpos pos lexbuf), Qname_ (s1, s2)

| string_literal_quote
| string_literal_single_quote ->
    let s = Ulexing.utf8_lexeme lexbuf in
    let s = String.sub s 1 (String.length s - 2) in
    (lexpos pos lexbuf), String_ (Rdf_utf8.utf8_unescape s)
| string_literal_long_quote
| string_literal_long_single_quote ->
    let s = Ulexing.utf8_lexeme lexbuf in
    let s = String.sub s 3 (String.length s - 6) in
    let pos = lexpos_nl pos lexbuf in
    pos, String_ (Rdf_utf8.utf8_unescape s)
| eof -> (lexpos pos lexbuf), EOF
| _ ->
  let s = Ulexing.utf8_lexeme lexbuf in
  let pos = Rdf_ulex.lexpos_nl pos lexbuf in
  let e = Failure (Printf.sprintf "Lexeme %S not handled" s) in
  raise (Rdf_ulex.Parse_error (e, pos))
;;



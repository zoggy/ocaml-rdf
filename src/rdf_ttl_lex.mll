(*********************************************************************************)
(*                OCaml-RDF                                                      *)
(*                                                                               *)
(*    Copyright (C) 2012-2013 Institut National de Recherche en Informatique     *)
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

let rec main line = lexer
| 'a' -> line, A
| "\r\n" -> main (line+1) lexbuf
| '\r' -> main line lexbuf
| '\n' -> main (line+1) lexbuf
| comment -> main line lexbuf
| ws -> main line lexbuf
| anon -> line, ANON
| '(' -> line, LEFT_PAR
| ')' -> line, RIGHT_PAR
| '[' -> line, LEFT_BRACKET
| ']' -> line, RIGHT_BRACKET
| ',' -> line, COMMA
| ';' -> line, SEMICOLON
| '.' -> line, DOT
| "@prefix" -> line, AT_PREFIX
| "@base" -> line, AT_BASE
| sparql_prefix -> line, PREFIX
| sparql_base -> line, BASE
| langtag ->
      let s = Ulexing.utf8_lexeme lexbuf in
      line, At_identifier s
| "^^" -> line, HATHAT
| boolean ->
      let s = Ulexing.utf8_lexeme lexbuf in
      line, Boolean s
| integer ->
      let s = Ulexing.utf8_lexeme lexbuf in
      line, Integer s
| decimal ->
      let s = Ulexing.utf8_lexeme lexbuf in
      line, Decimal s
| double ->
      let s = Ulexing.utf8_lexeme lexbuf in
      line, Double s
| iriref ->
      let s = Ulexing.utf8_lexeme lexbuf in
      let iri = String.sub s 1 (String.length s - 2) in
      let iri = Rdf_utf8.utf8_unescape iri in
      line, Iriref_ iri
| pname_ns ->
      let s = Ulexing.utf8_lexeme lexbuf in
      (*prerr_endline (Printf.sprintf "pname_ns %s" s);*)
      line, Identifier (String.sub s 0 (String.length s - 1))
| blank_node_label ->
  let s = Ulexing.utf8_lexeme lexbuf in
  let id = String.sub s 2 (String.length s - 2) in
  line, Bname id
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
  line, Qname_ (s1, s2)

| string_literal_quote
| string_literal_single_quote ->
    let s = Ulexing.utf8_lexeme lexbuf in
    let s = String.sub s 1 (String.length s - 2) in
    let line = line + Rdf_utf8.utf8_count_nl s in
    line, String_ (Rdf_utf8.utf8_unescape s)
| string_literal_long_quote
| string_literal_long_single_quote ->
    let s = Ulexing.utf8_lexeme lexbuf in
    let s = String.sub s 3 (String.length s - 6) in
    let line = line + Rdf_utf8.utf8_count_nl s in
    line, String_ (Rdf_utf8.utf8_unescape s)
| eof -> line, EOF
| _ ->
  let s = Ulexing.utf8_lexeme lexbuf in
  failwith (Printf.sprintf "Lexeme %S not handled" s)
;;



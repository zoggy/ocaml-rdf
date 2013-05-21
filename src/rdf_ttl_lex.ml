(** *)

open Rdf_ttl_parser;;

let regexp hex = [0x30-0x39] | [0x41-0x46]
let regexp character = "\\u" hex hex hex hex | "\\U" hex hex hex hex hex hex hex hex | '\\' | [0x20-0x5B] | [0x5D-0x10FFFF]
let regexp character_noquotes = "\\u" hex hex hex hex | "\\U" hex hex hex hex hex hex hex hex | '\\' | [0x20-0x21] | [0x23-0x5B] | [0x5D-0x10FFFF]
let regexp character_nogt = "\\u" hex hex hex hex | "\\U" hex hex hex hex hex hex hex hex | '\\' | [0x20-0x3D] | [0x3F-0x5B] | [0x5D-0x10FFFF]
let regexp echaracter = character | '\t' | '\n' | '\r'
let regexp echaracter_noquotes = character_noquotes | '\t' | '\n' | '\r'
let regexp lcharacter = echaracter | '\"' | 0x9 | 0xA | 0xD
let regexp scharacter = echaracter_noquotes | "\\\""
let regexp longstring = 0x22 0x22 0x22 lcharacter* 0x22 0x22 0x22
let regexp string = 0x22 scharacter* 0x22
let regexp quoted_string = string | longstring

let regexp ucharacter = ( character_nogt) | "\\>"
let regexp relativeURI = ucharacter*
let regexp nameStartChar_nounderscore = ['A'-'Z'] | ['a'-'z'] | [0x00C0-0x00D6] | [0x00D8-0x00F6] | [0x00F8-0x02FF] | [0x0370-0x037D] | [0x037F-0x1FFF] | [0x200C-0x200D] | [0x2070-0x218F] | [0x2C00-0x2FEF] | [0x3001-0xD7FF] | [0xF900-0xFDCF] | [0xFDF0-0xFFFD] | [0x10000-0xEFFFF]

let regexp nameStartChar = nameStartChar_nounderscore | '_'
let regexp nameChar = nameStartChar | '-' | ['0'-'9'] | 0x00B7 | [0x0300-0x036F] | [0x203F-0x2040]
let regexp prefixName = ( nameStartChar_nounderscore ) nameChar*
let regexp name = nameStartChar nameChar*

let main = lexer
| _ -> assert false
;;
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

type error =
| Parse_error of int * int * string

exception Error of error

let string_of_error = function
  Parse_error (start, stop, s) ->
    Printf.sprintf "characters %d-%d: parse error on lexeme %S" start stop s
;;

let parse_from_lexbuf lexbuf =
  let parse = Rdf_ulex.menhir_with_ulex Rdf_sparql_parser.query Rdf_sparql_lex.main in
  let q =
    try parse lexbuf
    with Rdf_sparql_parser.Error ->
        let (start, stop) = Ulexing.loc lexbuf in
        let lexeme = Ulexing.utf8_lexeme lexbuf in
        raise (Error (Parse_error (start, stop, lexeme)))
  in
  q

let parse_from_string s =
  let lexbuf = Ulexing.from_utf8_string s in
  parse_from_lexbuf lexbuf
;;

let parse_from_file file =
  let ic = open_in file in
  let lexbuf = Ulexing.from_utf8_channel ic in
  try parse_from_lexbuf lexbuf
  with e ->
      close_in ic;
      raise e
;;

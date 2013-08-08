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

open Rdf_sparql_types;;
open Rdf_sparql_eval;;

type error =
| Parse_error of int * int * string
| Exception of exn

exception Error of error

let string_of_error = function
  Parse_error (start, stop, s) ->
    Printf.sprintf "characters %d-%d: %s" start stop s
| Exception e ->
    match e with
      Unbound_variable v ->
        Printf.sprintf "%sUnbound variable %S"
          (Rdf_loc.string_of_loc v.var_loc) v.var_name
    | Not_a_integer lit ->
        "Not an integer: "^(Rdf_node.string_of_literal lit)
    | Not_a_double_or_decimal lit ->
        "Not an double: "^(Rdf_node.string_of_literal lit)
    | Type_mismatch (v1, v2) -> (* FIXME: show values *)
        "Type mismatch"
    | Invalid_fun_argument uri ->
        "Invalid argument for function "^(Rdf_uri.string uri)
    | Unknown_fun uri ->
        "Unknown function "^(Rdf_uri.string uri)
    | Invalid_built_in_fun_argument (name, _) ->
        "Invalid argument list for builtin function "^name
    | Unknown_built_in_fun name ->
        "Unknown builtit function "^name
    | No_term ->
        "No term"
    | Cannot_compare_for_datatype uri ->
        "Cannot compare values of datatype "^(Rdf_uri.string uri)
    | Unhandled_regex_flag c ->
        "Unhandled regexp flag "^(String.make 1 c)
    | Incompatible_string_literals (v1, v2) -> (* FIXME: show values *)
        "Incompatible string literals"
    | Empty_set name ->
        "Empty set in function "^name
    | Failure msg ->
         msg
    | e -> Printexc.to_string e
;;

let parse_from_lexbuf lexbuf =
  let parse = Rdf_ulex.menhir_with_ulex Rdf_sparql_parser.query Rdf_sparql_lex.main in
  let q =
    try parse lexbuf
    with
    (*| MenhirLib.TableInterpreter.Accept _ *)
    | Rdf_sparql_parser.Error ->
        let (start, stop) = Ulexing.loc lexbuf in
        let lexeme = Ulexing.utf8_lexeme lexbuf in
        let msg = Printf.sprintf "parse error on lexeme %S" lexeme in
        raise (Error (Parse_error (start, stop, msg)))
    | Failure msg ->
        let (start, stop) = Ulexing.loc lexbuf in
        raise (Error (Parse_error (start, stop, msg)))
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

let string_of_query q =
  let b = Buffer.create 256 in
  Rdf_sparql_print.print_query b q ;
  Buffer.contents b
;;
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

(** This code is adapted from CCSS: file ccss.ml *)

let lexpos pos lexbuf =
  let s = Ulexing.utf8_lexeme lexbuf in
  { pos with
    Lexing.pos_cnum = pos.Lexing.pos_cnum + (Rdf_utf8.utf8_length s);
  }
;;

let lexpos_nl pos lexbuf =
  let s = Ulexing.utf8_lexeme lexbuf in
  let len = String.length s in
  let rec iter pos i =
    if i < len then
      let pos =
        match s.[i] with
          '\n' ->
            let c = pos.Lexing.pos_cnum + 1 in
            { pos with
              Lexing.pos_bol = c ;
              Lexing.pos_cnum = c ;
              Lexing.pos_lnum = pos.Lexing.pos_lnum + 1 ;
            }
        | _ ->
            { pos with Lexing.pos_cnum = pos.Lexing.pos_cnum + 1 }
      in
      iter pos (i+(Rdf_utf8.utf8_nb_bytes_of_char s.[i]))
    else
      pos
  in
  iter pos 0
;;

exception Parse_error of exn * Lexing.position

let menhir_with_ulex menhir_parser lexer ?(fname="") lexbuf =
	let position = ref
		{
      Lexing.pos_fname = fname ;
      pos_lnum = 1;
      pos_bol = 0;
      pos_cnum = 0;
    }
  in
  let lexer_maker () =
    let ante_position = !position in
    let (pos, token) = lexer !position lexbuf in
    position := pos ;
    (token, ante_position, !position)
  in
  let revised_parser = MenhirLib.Convert.Simplified.traditional2revised menhir_parser in
  try revised_parser lexer_maker
  with
    Parse_error _ as e -> raise e
  | e -> raise (Parse_error (e, !position))
;;

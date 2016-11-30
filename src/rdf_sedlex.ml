(*********************************************************************************)
(*                OCaml-RDF                                                      *)
(*                                                                               *)
(*    Copyright (C) 2012-2016 Institut National de Recherche en Informatique     *)
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

exception Parse_error of exn * Lexing.position

let nl_char = Uchar.of_char '\n'

let update_pos pos str =
  let open Lexing in
  let f pos i = function
  | `Malformed msg -> 
      let exn = Failure msg in
      raise (Parse_error (exn, pos))
  | `Uchar c when Uchar.equal c nl_char ->
      let bol = pos.pos_cnum in
      { pos with
        pos_lnum = pos.pos_lnum + 1;
        pos_bol = bol ;
        pos_cnum = pos.pos_cnum + 1 ;
      }
  | _ -> { pos with pos_cnum = pos.pos_cnum + 1}
  in
  Uutf.String.fold_utf_8 f pos str

let upd pos lexbuf = update_pos pos (Sedlexing.Utf8.lexeme lexbuf)


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

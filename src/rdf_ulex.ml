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

(** This code is adapted from CCSS: file ccss.ml *)


let menhir_with_ulex menhir_parser lexer lexbuf =
	let position = ref
		{
      Lexing.pos_fname = "";
      pos_lnum = 1;
      pos_bol = 0;
      pos_cnum = 0;
    }
  in
  let lexer_maker () =
    let ante_position = !position in
    (*
    let (nlines, token) = lexer 0 lexbuf in
    let () = position := {!position with Lexing.pos_lnum = !position.Lexing.pos_lnum + nlines;} in
    let post_position = !position in
    (token, ante_position, post_position)
    *)
    let token = lexer lexbuf in
    (token, ante_position, ante_position)
  in
  let revised_parser = MenhirLib.Convert.Simplified.traditional2revised menhir_parser in
  revised_parser lexer_maker
;;

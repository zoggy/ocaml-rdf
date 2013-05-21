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

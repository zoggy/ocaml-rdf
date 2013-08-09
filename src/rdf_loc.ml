(** *)


(* Print the location in some way or another *)

open Format
open Lexing

type loc =
  { loc_start : Lexing.position ;
    loc_end : Lexing.position ;
  }

let dummy_loc = {
    loc_start = Lexing.dummy_pos ;
    loc_end = Lexing.dummy_pos ;
  }


let source_info_string s start stop =
  let bols = List.rev (Rdf_utf8.utf8_string_get_bol s) in
  let rec search cnum = function
    [] -> (1,0)
  | (line,bol) :: q ->
      if cnum >= bol then (line, bol) else search cnum q
  in
  let pos p =
    let (line, bol) = search p bols in
    { pos_cnum = p ; pos_fname = "" ;
      pos_bol = bol ; pos_lnum = line ;
    }
  in
  { loc_start = pos start ; loc_end = pos stop }
;;

let source_info_file file start stop =
  let s = Rdf_misc.string_of_file file in
  let loc = source_info_string s start stop in
  {
    loc_start = {loc.loc_start with pos_fname = file } ;
    loc_end = { loc.loc_end with pos_fname = file } ;
  }

let (msg_file, msg_line, msg_char, msg_chars, msg_to, msg_colon, msg_head) =
  ("File \"", "\", line ", ", character ", ", characters ", "-", ":", "")

(* return file, line, char from the given position *)
let get_pos_info pos =
  let (filename, linenum, linebeg) =
    if pos.pos_fname = "" then
      ("", -1, 0)
    else
      (pos.pos_fname, pos.pos_lnum, pos.pos_bol)
  in
  (filename, linenum, pos.pos_cnum - linebeg)
;;

let print ppf loc =
  let (file, line, startchar) = get_pos_info loc.loc_start in
  let endchar = loc.loc_end.pos_cnum - loc.loc_start.pos_cnum + startchar in
  let (startchar, endchar) =
    if startchar < 0 then (0, 1) else (startchar, endchar)
  in
  fprintf ppf "%s%s%s%i" msg_file file msg_line line;
  if startchar <> endchar then
    begin
      fprintf ppf "%s%i" msg_chars startchar;
      fprintf ppf "%s%i%s@.%s" msg_to endchar msg_colon msg_head
    end
  else
    begin
      fprintf ppf "%s%i" msg_char startchar;
      fprintf ppf "%s@.%s" msg_colon msg_head
    end
;;

let string_of_loc loc =
  print Format.str_formatter loc;
  Format.flush_str_formatter ()
;;


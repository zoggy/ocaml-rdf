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

let utf8_nb_bytes_of_char c =
  let n = Char.code c in
  if n < 0b10000000 then
    1
  else if n < 0b11100000 then
      2
    else if n < 0b11110000 then
        3
      else
        4

let utf8_index_of_char s c =
  let cpt = ref 0 in
  let current = ref 0 in
  let len = String.length s in
  while !current < c && !cpt < len do
    cpt := !cpt + utf8_nb_bytes_of_char s.[!cpt];
    incr current;
  done;
  if !current = c then
    !cpt
  else
    raise Not_found

let utf8_char_of_index s i =
  let len = String.length s in
  if i >= len or i < 0 then
    invalid_arg "utf8_char_from_index"
  else
    begin
      let char_count = ref (-1) in
      let pos = ref 0 in
      while !pos <= i && !pos < len do
        incr char_count;
        pos := !pos + utf8_nb_bytes_of_char s.[!pos]
      done;
      !char_count
    end

let utf8_string_length s =
  let len = String.length s in
  let rec iter acc n =
    if n >= len then
      acc
    else
      iter (acc+1) (n + (utf8_nb_bytes_of_char s.[n]))
  in
  iter 0 0

let utf8_substr s pos l =
  (*prerr_endline (Printf.sprintf "utf8_substr pos=%d l=%d" pos l);*)
  let inv () = invalid_arg "Rdf_utf8.utf8_substr" in
  let len = String.length s in
  if pos < 0 || l >= len then inv ();
  let start = ref (-1) in
  let stop = ref (-1) in
  let rec iter i n =
    (*print_endline (Printf.sprintf "iter i=%d, n=%d" i n);*)
    if n = pos then start := i ;
    if n = pos + l then
      stop := i
    else
      (
       if n >= len then inv () ;
       let size = utf8_nb_bytes_of_char s.[i] in
       iter (i+size) (n+1)
      )
  in
  iter 0 0 ;
  (*print_endline (Printf.sprintf "start=%d, stop=%d" !start !stop);*)
  String.sub s !start (!stop - !start)
;;
(*
let () = print_endline (utf8_substr "abécédé" 2 4);;
let () = print_endline (utf8_substr "abécédé" 4 0);;
let () = print_endline (utf8_substr "abécédé" 2 5);;
*)

let utf8_is_prefix s1 s2 =
  let len1 = utf8_string_length s1 in
  let len2 = utf8_string_length s2 in
  (len1 >= len2) && (String.sub s1 0 len2) = s2
;;

let utf8_is_suffix s1 s2 =
  let len1 = utf8_string_length s1 in
  let len2 = utf8_string_length s2 in
  if len1 = len2 then
    s1 = s2
  else
    (len1 >= len2) && (String.sub s1 (len1 - len2) len2) = s2
;;

let utf8_substr_pos s1 s2 =
  let ulen1 = utf8_string_length s1 in
  let ulen2 = utf8_string_length s2 in
  let len2 = String.length s2 in
  let rec iter i n =
    if ulen1 - n >= ulen2 then
      (
       if String.sub s1 i len2 = s2 then
         Some i
       else
         (
          let size = utf8_nb_bytes_of_char s1.[i] in
          iter (i+size) (n+1)
         )
      )
    else
      None
  in
  iter 0 0
;;

let utf8_contains s1 s2 = utf8_substr_pos s1 s2 <> None;;

let utf8_strbefore s1 = function
  "" -> s1
| s2 ->
    match utf8_substr_pos s1 s2 with
      None
    | Some 0 -> ""
    | Some n -> String.sub s1 0 n
;;

let utf8_strafter s1 = function
  "" -> s1
| s2 ->
    match utf8_substr_pos s1 s2 with
      None -> ""
    | Some 0 -> s1
    | Some n ->
        let start = n + String.length s2 in
        String.sub s1 start (String.length s1 - start)
;;


(** conversions algorithm from [http://en.wikipedia.org/wiki/UTF-8]. *)
let utf8_char_of_code n =
  if n < 128 then
    String.make 1 (Char.chr n)
  else
    let z_mask = 0b00111111 in
    let z_part = (n land z_mask) in
    let z = 0b10000000 lor z_part in
    if n <= 0x0007FF then
      let y_mask = 0b0000011111000000 in
      let y_part = (n land y_mask) lsr 6 in
      let y = 0b11000000 lor y_part in
      Printf.sprintf "%c%c" (Char.chr y) (Char.chr z)
    else
      let y_mask = 0b111111000000 in
      let y_part = (n land y_mask) lsr 6 in
      let y = 0b10000000 lor y_part in
      if n <= 0x00FFFF then
        let x_mask = 0b1111 lsl 12 in
        let x_part = (n land x_mask) lsr 12 in
        let x = 0b11100000 lor x_part in
        Printf.sprintf "%c%c%c" (Char.chr x) (Char.chr y) (Char.chr z)
      else
        if n <= 0x10FFFF then
          let x_mask = 0b111111 lsl 12 in
          let x_part = (n land x_mask) lsr 12 in
          let x = 0b10000000 lor x_part in
          let w_mask = 0b111 lsl 18 in
          let w_part = (n land w_mask) lsr 18 in
          let w = 0b11110000 lor w_part in
          Printf.sprintf "%c%c%c%c" (Char.chr w) (Char.chr x) (Char.chr y) (Char.chr z)
        else
          failwith (Printf.sprintf "UTF-8 code out of range: %x" n)
;;

let utf8_string_get_bol =
  let rec iter acc len s line char i =
    if i >= len then
      List.rev acc
    else
      (
       let size = utf8_nb_bytes_of_char s.[i] in
       match s.[i] with
         '\n' -> iter ((line+1,char)::acc) len s (line+1) (char+1) (i+size)
       | _ -> iter acc len s line (char+1) (i+size)
      )
  in
  fun s -> iter [] (String.length s) s 1 0 0
;;

let utf8_count_nl =
  let rec iter len s n i =
    if i >= len then n
    else
      (let size = utf8_nb_bytes_of_char s.[i] in
       match s.[i] with
         '\n' -> iter len s (n+1) (i+size)
       | _ -> iter len s n (i+size)
      )
  in
  fun s -> iter (String.length s) s 0 0
;;

(** Escape some characters by \n, \r, \b, \t, \quotes and \\ but do not escape \u. *)
let utf8_escape =
  let rec iter b len s i =
    if i >= len then
      ()
    else
      begin
        let size = utf8_nb_bytes_of_char s.[i] in
        begin
          match s.[i] with
          | '\b' -> Buffer.add_string b "\\b"
          | '\n' -> Buffer.add_string b "\\n"
          | '\r' -> Buffer.add_string b "\\r"
          | '\t' -> Buffer.add_string b "\\t"
          | '"' -> Buffer.add_string b "\\\""
          | '\\' when i < len - 1 && s.[i] <> 'u' && s.[i] <> 'U' ->
              Buffer.add_string b "\\\\"
          | c ->
            Buffer.add_string b (String.sub s i size);
         end;
         iter b len s (i+size)
       end
  in
  fun s ->
    let len = String.length s in
    let b = Buffer.create len in
    iter b len s 0 ;
    Buffer.contents b
;;


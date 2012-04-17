(*********************************************************************************)
(*                Chamo                                                          *)
(*                                                                               *)
(*    Copyright (C) 2003-2012 Institut National de Recherche en Informatique     *)
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

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
    cpt := !cpt + utf8_nb_bytes_of_char (String.get s !cpt) ;
    incr current;
  done;
  if !current = c then
    !cpt
  else
    raise Not_found

let utf8_char_of_index s i =
  let len = String.length s in
  if i >= len || i < 0 then
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

let utf8_length s =
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
  let len1 = utf8_length s1 in
  let len2 = utf8_length s2 in
  (len1 >= len2) && (String.sub s1 0 len2) = s2
;;

let utf8_is_suffix s1 s2 =
  let len1 = utf8_length s1 in
  let len2 = utf8_length s2 in
  if len1 = len2 then
    s1 = s2
  else
    (len1 >= len2) && (String.sub s1 (len1 - len2) len2) = s2
;;

let utf8_substr_pos s1 s2 =
  let ulen1 = utf8_length s1 in
  let ulen2 = utf8_length s2 in
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
  let n_int = Int32.to_int n in
  if Int32.compare n (Int32.of_int 128) < 0 then
    String.make 1 (Char.chr n_int)
  else
    let z_mask = Int32.of_int 0b00111111 in
    let z_part = Int32.logand n z_mask in
    let z = Int32.logor (Int32.of_int 0b10000000) z_part in
    if Int32.compare n (Int32.of_int 0x0007FF) <= 0 then
      let y_mask = Int32.of_int 0b0000011111000000 in
      let y_part = Int32.shift_right_logical (Int32.logand n y_mask) 6 in
      let y = Int32.logor (Int32.of_int 0b11000000) y_part in
      let s = Bytes.of_string "12" in
      Bytes.set s 0 (Char.chr (Int32.to_int y)) ;
      Bytes.set s 1 (Char.chr (Int32.to_int z)) ;
      Bytes.to_string s
    else
      let y_mask = Int32.of_int 0b111111000000 in
      let y_part = Int32.shift_right_logical (Int32.logand n y_mask) 6 in
      let y = Int32.logor (Int32.of_int 0b10000000) y_part in
      if Int32.compare n (Int32.of_int 0x00FFFF) <= 0 then
        let x_mask = Int32.of_int (0b1111 lsl 12) in
        let x_part = Int32.shift_right_logical (Int32.logand n x_mask) 12 in
        let x = Int32.logor (Int32.of_int 0b11100000) x_part in
        let s = Bytes.of_string "123" in
        Bytes.set s 0 (Char.chr (Int32.to_int x)) ;
        Bytes.set s 1 (Char.chr (Int32.to_int y)) ;
        Bytes.set s 2 (Char.chr (Int32.to_int z)) ;
        Bytes.to_string s
      else
        if Int32.compare n (Int32.of_int 0x10FFFF) <= 0 then
          let x_mask = Int32.of_int (0b111111 lsl 12) in
          let x_part = Int32.shift_right_logical (Int32.logand n x_mask) 12 in
          let x = Int32.logor (Int32.of_int 0b10000000) x_part in
          let w_mask = Int32.of_int (0b111 lsl 18) in
          let w_part = Int32.shift_right_logical (Int32.logand n w_mask) 18 in
          let w = Int32.logor (Int32.of_int 0b11110000) w_part in
          let s = Bytes.of_string "1234" in
          Bytes.set s 0 (Char.chr (Int32.to_int w)) ;
          Bytes.set s 1 (Char.chr (Int32.to_int x)) ;
          Bytes.set s 2 (Char.chr (Int32.to_int y)) ;
          Bytes.set s 3 (Char.chr (Int32.to_int z)) ;
          Bytes.to_string s
        else
          failwith ("UTF-8 code out of range: "^ (Int32.to_string n))
;;

let utf8_get_bol =
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

let utf8_char_escape_string dest str =
  let len = String.length str in
  Buffer.add_string dest "\\";
  Buffer.add_char dest (if len <= 2 then 'u' else 'U');
  String.iter
    (fun c -> Printf.bprintf dest "%02X" (Char.code c))
    str

let utf8_escape =
(*  let rec iter b len s i =
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
          | '\x0c' -> Buffer.add_string b "\\f"
          | '"' -> Buffer.add_string b "\\\""
          | '\'' -> Buffer.add_string b "'"
          | '\\' when i < len - 1 && s.[i+1] <> 'u' && s.[i+1] <> 'U' ->
              Buffer.add_string b "\\\\"
          | '\\' when i = len - 1 ->
              (* string ending with '\', escape it too *)
              Buffer.add_string b "\\\\"
          | c ->
            if size > 1 then
              utf8_char_escape_string b (String.sub s i size)
            else
              Buffer.add_string b (String.sub s i size);
         end;
         iter b len s (i+size)
       end
  in*)
  let f b i = function
    `Malformed str -> Buffer.add_string b str; b
  | `Uchar cp ->
       match cp with
         0x08 (* '\b' *) -> Buffer.add_string b "\\b"; b
       | 0x09 (* '\t' *) -> Buffer.add_string b "\\t"; b
       | 0x0A (* '\n' *) -> Buffer.add_string b "\\n"; b
       | 0x0C (* '\f' *) -> Buffer.add_string b "\\f"; b
       | 0x0D (* '\r' *) -> Buffer.add_string b "\\r"; b
       | 0x22 (* quote *) -> Buffer.add_string b "\\\""; b
       | 0x5C (* \ *) -> Buffer.add_string b "\\\\"; b
       | n when n < 128 -> Uutf.Buffer.add_utf_8 b cp; b
       | n ->
           let str = Printf.sprintf "%x" n in
           if String.length str > 4 then
             Printf.bprintf b "\\U%08X" n
           else
             Printf.bprintf b "\\u%04X" n;
           b
  in
  fun s ->
    let b = Buffer.create (String.length s) in
    ignore(Uutf.String.fold_utf_8 f b s) ;
    Buffer.contents b
;;

let utf8_unescape =
 let int32_of_chars s start len =
   let s2 = "0x"^(String.sub s start len) in
   try Int32.of_string s2
   with _ -> failwith ("Invalid UTF8 code: "^s)
 in
 let escaped_chars = List.fold_left
   (fun map (c1, c2) -> Rdf_types.CMap.add c1 c2 map)
   Rdf_types.CMap.empty
   [ 'b', '\b' ;
     'f', Char.chr 0x0c ;
     'n', '\n' ;
     'r', '\r' ;
     't', '\t' ;
     '"', '"' ;
     '\'', '\'' ;
     '\\', '\\' ;
   ]
 in
 let rec iter b len s i =
   if i >= len then
      ()
    else
      begin
        let size = utf8_nb_bytes_of_char s.[i] in
        let next =
          match size, s.[i] with
            1, '\\' ->
              if i + size < len then
                try
                  let c = Rdf_types.CMap.find s.[i+size] escaped_chars in
                  Buffer.add_char b c;
                  i+size+1
                with
                  Not_found ->
                    match s.[i+size] with
                    | 'u' when i+size+4 < len ->
                        let code = int32_of_chars s (i+size+1) 4 in
                        Buffer.add_string b (utf8_char_of_code code);
                        i+size+5
                    | 'U' when i+size+8 < len ->
                        let code = int32_of_chars s (i+size+1) 8 in
                        Buffer.add_string b (utf8_char_of_code code);
                        i+size+9
                    | c ->
                      Buffer.add_char b s.[i];
                      i + size
              else
                (
                 Buffer.add_char b s.[i] ;
                 i + size
                )
          | _ ->
              Buffer.add_string b (String.sub s i size);
              i + size
        in
        iter b len s next
      end
 in
 fun s ->
   let len = String.length s in
    let b = Buffer.create len in
    iter b len s 0 ;
    Buffer.contents b
;;

let utf8_lowercase s =
  let b = Bytes.of_string s in
  let len = String.length s in
  let i = ref 0 in
  while !i < len do
    match utf8_nb_bytes_of_char s.[!i] with
      1 -> Bytes.set b !i (Char.lowercase s.[!i]); incr i
    | n -> i := !i + n
  done;
  Bytes.to_string b
;;

let utf8_uppercase s =
  let b = Bytes.of_string s in
  let len = String.length s in
  let i = ref 0 in
  while !i < len do
    match utf8_nb_bytes_of_char s.[!i] with
      1 -> Bytes.set b !i (Char.uppercase s.[!i]); incr i
    | n -> i := !i + n
  done;
  Bytes.to_string b
;;





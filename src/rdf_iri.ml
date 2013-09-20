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

let gen_delims = "[:/?#[\\]@]";;
let sub_delims = "[!$&'()*+,;=]";;

let pct_encoded = "%[0-9a-fA-F]";;
let ucschar = "\\x{A0}-\\x{D7FF}\\x{F900}-\\x{FDCF}\\x{FDF0}-\\x{FFEF}\\x{10000}-\\x{1FFFD} / \\x{20000}-\\x{2FFFD} / \\x{30000}-\\x{3FFFD}\\x{40000}-\\x{4FFFD} / \\x{50000}-\\x{5FFFD} / \\x{60000}-\\x{6FFFD}\\x{70000}-\\x{7FFFD} / \\x{80000}-\\x{8FFFD} / \\x{90000}-\\x{9FFFD}\\x{A0000}-\\x{AFFFD} / \\x{B0000}-\\x{BFFFD} / \\x{C0000}-\\x{CFFFD}\\x{D0000}-\\x{DFFFD} / \\x{E1000}-\\x{EFFFD}";;

let iprivate = "[\\x{E000}-\\x{F8FF}\\x{F0000}-\\x{FFFFD}\\x{100000}-\\x{10FFFD}]"

let iunreserved = "a-zA-Z0-9\\-._~"^ucschar

let ipchar = "["^iunreserved^sub_delims^":@]|"^pct_encoded

let regexp s =
  try Pcre.regexp ~flags: [`UTF8] s
  with Pcre.Error(Pcre.BadPattern (s, n)) ->
        failwith (Printf.sprintf "Bad PCRE pattern on char %d: %s" n s)

let scheme = "^(?<scheme>[a-zA-Z0-9\\-\\d+.]+)" ;;
let re_scheme = Pcre.regexp scheme;;


exception Invalid_iri of string * string

let invalid_iri str msg = raise (Invalid_iri (str, msg));;

type iri_ = {
    scheme : string ;
    user : string option ;
    host : string option ;
    port : int option ;
    path : string list ;
    query : string option ;
    fragment : string option ;
  }

type iri = string

let re_iri =regexp
    (scheme^":((//((<?<user>[^@])+@)?(?<host>[^:/#]*)(:(?<port>\\d+))?(/(?<path1>[^?#]*)))|(?<path2>[^?#]*))(\\?(?<query>[^#]*))?(#(?<fragment>.*))?")
;;

(*
let pct_decode =
  let rec iter b len s i =
    if i >= len then
      ()
    else
      begin
        let i =
          match s.[i] with
            '%' when i+2 < len ->
              begin
                try
                  let n = int_of_string ("0x"^(String.sub s (i+1) 2)) in
                  Buffer.add_char b (Char.chr n);
                  i+3
                with
                  _ -> Buffer.add_char b s.[i]; i+1
              end
          | _ ->
              let size = Rdf_utf8.utf8_nb_bytes_of_char s.[i] in
              if size = 1 then
                Buffer.add_char b s.[i]
              else
                Buffer.add_substring b s i size ;
              i+size
        in
        iter b len s i
      end
  in
  fun s ->
    let len = String.length s in
    let b = Buffer.create len in
    iter b len s 0;
    Buffer.contents b
;;
*)

let pct_encode =
  let rec iter safe_chars b len s i =
   if i >= len then
      ()
    else
      begin
        if safe_chars.(Char.code s.[i]) then
          Buffer.add_char b s.[i]
        else
          begin
            Printf.bprintf b "%%%02X" (Char.code s.[i])
          end;
        iter safe_chars b len s (i+1)
      end
  in
  fun safe_chars s ->
    let len = String.length s in
    let b = Buffer.create len in
    iter safe_chars b len s 0;
    Buffer.contents b
;;

let safe_chars =
  let f n =
    match Char.chr n with
      'A'..'Z' | 'a'..'z' | '0'..'9' | '-' | '.' | '%' -> true
    | _ -> false
  in
  Array.init 256 f
;;
let scheme_safe_chars =
  let a = Array.copy safe_chars in
  a.(Char.code '+') <- true;
  a
;;

let sub_delims =
  [| '!' ; '$' ; '&' ; '\'' ; '(' ; ')' ;
     '*' ; '+' ; ',' ; ';' ; '=' |]
;;

let user_safe_chars =
  let a = Array.copy safe_chars in
  a.(Char.code ':') <- true;
  a.(Char.code '~') <- true;
  a.(Char.code '_') <- true;
  Array.iter (fun c -> a.(Char.code c) <- true) sub_delims ;
  a
;;

let host_safe_chars =
  let a = Array.copy safe_chars in
  a.(Char.code '~') <- true;
  a.(Char.code '_') <- true;
  a.(Char.code '[') <- true;
  a.(Char.code ']') <- true;
  a.(Char.code ':') <- true;
  Array.iter (fun c -> a.(Char.code c) <- true) sub_delims ;
  a
;;

let path_safe_chars =
  let a = Array.copy safe_chars in
  a.(Char.code '~') <- true;
  a.(Char.code '_') <- true;
  a.(Char.code ':') <- true;
  a.(Char.code '@') <- true;
  Array.iter (fun c -> a.(Char.code c) <- true) sub_delims ;
  a
;;


let query_safe_chars =
  let a = Array.copy safe_chars in
  a.(Char.code '~') <- true;
  a.(Char.code '_') <- true;
  a.(Char.code ':') <- true;
  a.(Char.code '@') <- true;
  a.(Char.code '?') <- true;
  a.(Char.code '/') <- true;
  Array.iter (fun c -> a.(Char.code c) <- true) sub_delims ;
  a
;;

let fragment_safe_chars =
  let a = Array.copy safe_chars in
  a.(Char.code '~') <- true;
  a.(Char.code '_') <- true;
  a.(Char.code ':') <- true;
  a.(Char.code '@') <- true;
  a.(Char.code '?') <- true;
  a.(Char.code '/') <- true;
  Array.iter (fun c -> a.(Char.code c) <- true) sub_delims ;
  a
;;

let parse s =
  let rex = re_iri in
  let subs =
    try Pcre.exec ~rex s
    with _ -> invalid_iri s "Scheme form not matched"
  in
  let scheme =
    try Pcre.get_named_substring rex "scheme" subs
    with _ -> invalid_iri s "No scheme"
  in
  let host =
    try Some (Pcre.get_named_substring rex "host" subs)
    with _ -> None
  in
  let user =
    try Some (Pcre.get_named_substring rex "user" subs)
    with _ -> None
  in
  let port =
    try Some (Pcre.get_named_substring rex "port" subs)
    with _ -> None
  in
  let port =
    match port with
      None -> None
    | Some p ->
        try Some (int_of_string p)
        with _ -> invalid_iri s "Invalid port"
  in
  let path =
    let s =
      try Pcre.get_named_substring rex "path1" subs
      with _ ->
          try Pcre.get_named_substring rex "path2" subs
          with _ -> invalid_iri s "No path"
    in
    Rdf_misc.split_string s ['/']
  in
  let query =
    try Some (Pcre.get_named_substring rex "query" subs)
    with _ -> None
  in
  let fragment =
    try Some (Pcre.get_named_substring rex "fragment" subs)
    with _ -> None
  in
  let iri = { scheme ; user ; host ; port ; path ; query ; fragment } in
  iri
;;

let to_string =
  let string_of_path encode l =
    let l =
      if encode then
        List.map (pct_encode path_safe_chars) l
      else
        l
    in
    String.concat "/" l
  in
  fun ?(encode=false) iri  ->
    let has_ihier = iri.host <> None in
    let b = Buffer.create 256 in
    Buffer.add_string b iri.scheme ;
    Buffer.add_string b ":" ;
    if has_ihier then Buffer.add_string b "//";
    (match iri.user with
       None -> ()
     | Some u ->
         Buffer.add_string b
           (if encode then pct_encode user_safe_chars u else u);
         Buffer.add_char b '@'
    );
    (match iri.host with
       None -> ()
     | Some s ->
         Buffer.add_string b
           (if encode then pct_encode host_safe_chars s else s)
    );
    (match iri.port with None -> () | Some n -> Buffer.add_string b (":"^(string_of_int n))) ;
    if has_ihier then Buffer.add_string b "/" ;
    Buffer.add_string b (string_of_path encode iri.path);
    (match iri.query with
       None -> ()
     | Some s ->
         Buffer.add_char b '?';
         Buffer.add_string b (if encode then pct_encode query_safe_chars s else s)
    );
    (match iri.fragment with
       None -> ()
     | Some s ->
         Buffer.add_char b '#' ;
         Buffer.add_string b (if encode then pct_encode fragment_safe_chars s else s)
    );
    Buffer.contents b
;;

let iri ?(check=true) s =
  if check then ignore(parse s) ;
  s
;;

let string s = s;;
let to_uri iri =
  let s = to_string ~encode: true (parse iri) in
  Rdf_uri.uri s
;;

let append ?check i s = iri ?check (i^s);;

let concat i s =
  let t = parse i in
  let t = { t with path = t.path @ [s] } in
  to_string t
;;

let path i = (parse i).path;;

let compare i1 i2 = String.compare i1 i2;;
let equal i1 i2 = compare i1 i2 = 0;;

module Iriset = Rdf_types.SSet;;
module Irimap = Rdf_types.SMap;;


  
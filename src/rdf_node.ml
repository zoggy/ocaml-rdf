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

let () = Random.self_init();;

type literal = {
    lit_value : string ;
    lit_language : string option ;
    lit_type : Rdf_uri.uri option ;
  }
type blank_id = string

type node =
  | Uri of Rdf_uri.uri
  | Literal of literal
  | Blank
  | Blank_ of blank_id



type triple = node * node * node

let string_of_blank_id id = id;;
let blank_id_of_string str = str;;

let node_of_uri_string s = Uri (Rdf_uri.uri s);;
let mk_literal ?typ ?lang v =
  { lit_value = v ; lit_language = lang ; lit_type = typ ; }
;;
let node_of_literal_string ?typ ?lang v =
  Literal (mk_literal ?typ ?lang v)
;;

let mk_literal_datetime ?(d=Unix.time()) () =
  let v = Netdate.mk_internet_date d in
  mk_literal ~typ: (Rdf_uri.uri "http://www.w3.org/2001/XMLSchema#dateTime") v
;;

let node_of_datetime ?d () =
  Literal (mk_literal_datetime ?d ())
;;

let datetime_of_literal lit = Netdate.parse lit.lit_value;;

let mk_literal_bool b =
  let v = if b then "1" else "0" in
  mk_literal ~typ: Rdf_rdf.xsd_boolean v
;;

let mk_literal_int n =
  mk_literal ~typ: Rdf_rdf.xsd_integer (string_of_int n)
;;

let mk_literal_double f =
  mk_literal ~typ: Rdf_rdf.xsd_double (string_of_float f)
;;

let node_of_int n = Literal (mk_literal_int n)
let node_of_double f = Literal (mk_literal_double f)
let node_of_bool b = Literal (mk_literal_bool b);;

let bool_of_literal lit =
  match lit.lit_value with
    "1" | "true" -> true
  | _ -> false
;;

(** We must not escape \u sequences used to encode UTF-8 characters.
  Since String.escaped escapes all '\\', then unescape "\\u" back to "\u".
*)
let unescape_backslash_u s =
  let len = String.length s in
  let b = Buffer.create len in
  let rec iter p =
    if p < len - 3 then
      match s.[p], s.[p+1], s.[p+2] with
        '\\', '\\', 'u' -> Buffer.add_string b "\\u" ; iter (p+3)
      | '\\', '\\', 'U' -> Buffer.add_string b "\\U" ; iter (p+3)
      | c, _, _ -> Buffer.add_char b c; iter (p+1)
    else if p < len then
        (
         Buffer.add_char b s.[p] ;
         iter (p+1)
        )
  in
  iter 0;
  Buffer.contents b
;;

let quote_str s =
  let s = String.escaped s in
  let s = unescape_backslash_u s in
  "\"" ^ s ^ "\""
;;

let string_of_literal lit =
  (quote_str lit.lit_value) ^
    (match lit.lit_language with
       None -> ""
     | Some l -> "@" ^ l
    ) ^
    (match lit.lit_type with
       None -> ""
     | Some t -> "^^<" ^ (Rdf_uri.string t) ^ ">"
    )

let string_of_node = function
| Uri uri -> "<" ^ (Rdf_uri.string uri) ^ ">"
| Literal lit -> string_of_literal lit
| Blank -> "_"
| Blank_ id ->  "_:" ^ (string_of_blank_id id)
;;

let int64_hash str =
  let digest = Digest.string str in
  (* use the same method as in librdf: use the 8 first bytes to
     get a 64 bits integer independant from the little/big endianness *)
  let hash = ref Int64.zero in
  for i = 0 to 7 do
    hash := Int64.add !hash (Int64.shift_left (Int64.of_int (Char.code digest.[i])) (i*8))
  done;
  !hash
;;

let node_hash = function
  Uri uri -> int64_hash ("R" ^ (Rdf_uri.string uri))
| Literal lit ->
    int64_hash (
     "L" ^
       lit.lit_value ^ "<" ^
       (Rdf_misc.string_of_opt lit.lit_language) ^ ">" ^
       (Rdf_misc.string_of_opt (Rdf_misc.map_opt Rdf_uri.string lit.lit_type))
    )
| Blank -> assert false
| Blank_ id -> int64_hash ("B" ^ (string_of_blank_id id))
;;

let compare node1 node2 =
  match node1, node2 with
    Uri uri1, Uri uri2 -> Rdf_uri.compare uri1 uri2
  | Uri _, _ -> 1
  | _, Uri _ -> -1
  | Literal lit1, Literal lit2 ->
      begin
        match Pervasives.compare lit1.lit_value lit2.lit_value with
          0 ->
            begin
              match Pervasives.compare lit1.lit_language lit2.lit_language with
                0 ->
                  begin
                    match lit1.lit_type, lit2.lit_type with
                      None, None -> 0
                    | None, _ -> 1
                    | _, None -> -1
                    | Some uri1, Some uri2 -> Rdf_uri.compare uri1 uri2
                  end
              | n -> n
            end
        | n -> n
      end
  | Literal _, _ -> 1
  | _, Literal _ -> -1
  | Blank, Blank -> 0
  | Blank, _ -> 1
      | _, Blank -> -1
  | Blank_ id1, Blank_ id2 ->
      Pervasives.compare
        (string_of_blank_id id1)
        (string_of_blank_id id2)

module Ord_type =
  struct
    type t = node
    let compare = compare
  end;;

module NSet = Set.Make (Ord_type);;

let lit_true = mk_literal_bool true
let lit_false = mk_literal_bool false

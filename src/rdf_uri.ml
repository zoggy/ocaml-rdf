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

let syntax =
  let stx = Hashtbl.find Neturl.common_url_syntax "http" in
  let stx = Neturl.partial_url_syntax stx in
  { stx with Neturl.url_enable_fragment = Neturl.Url_part_allowed }
;;

type uri =
  | String of string
  | Parsed of Neturl.url;;

exception Invalid_url of string

let string = function
  String s -> s
| Parsed u -> Neturl.string_of_url u
;;

let parse s =
  try Neturl.url_of_string syntax s
  with Neturl.Malformed_URL ->
      raise (Invalid_url s)
;;
let get_parsed = function String s -> parse s | Parsed u -> u ;;

let uri ?(check=true) s = if check then Parsed (parse s) else String s;;

let concat uri s =
  match uri with
    Parsed uri ->
      let path = (Neturl.url_path uri)@[s] in
      Parsed (Neturl.modify_url ~path uri)
  | String u -> String (u^"/"^s)
;;

let parent uri =
  let uri = get_parsed uri in
  let path = Neturl.url_path uri in
  let path =
    match List.rev path with
      [] -> []
    | _ :: q -> List.rev q
  in
  Parsed (Neturl.modify_url ~path uri)
;;

let set_fragment uri fragment =
  let uri = get_parsed uri in
  Parsed (Neturl.modify_url ~fragment uri)
;;

let path uri =
  Neturl.url_path (get_parsed uri)
;;

let compare uri1 uri2 = Pervasives.compare (string uri1) (string uri2);;

let equal u1 u2 = compare u1 u2 = 0;;

let neturl uri = get_parsed uri;;
let of_neturl uri = Parsed uri;;

module Urimap = Map.Make (struct type t = uri let compare = compare end);;
module Uriset = Set.Make (struct type t = uri let compare = compare end);;

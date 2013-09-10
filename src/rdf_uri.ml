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

type uri = string


exception Invalid_url of string

let string x = x;;

let neturl s =
  try Neturl.url_of_string syntax s
  with Neturl.Malformed_URL ->
      raise (Invalid_url s)
;;

let of_neturl uri = Neturl.string_of_url uri;;

let uri ?(check=true) s =
  if check then (ignore (neturl s); s) else s;;

let concat uri s =
  let uri = neturl uri in
  let path = (Neturl.url_path uri)@[s] in
  of_neturl (Neturl.modify_url ~path uri)
;;

let append ?check u s =
  let u = neturl u in
  let path = String.concat "/" (Neturl.url_path u) in
  uri ?check (path^s);;

let parent uri =
  let uri = neturl uri in
  let path = Neturl.url_path uri in
  let path =
    match List.rev path with
      [] -> []
    | _ :: q -> List.rev q
  in
  of_neturl (Neturl.modify_url ~path uri)
;;

let set_fragment uri fragment =
  let uri = neturl uri in
  of_neturl (Neturl.modify_url ~fragment uri)
;;

let path uri =
  Neturl.url_path (neturl uri)
;;

let compare = String.compare

let equal u1 u2 = u1 = u2 ;;


module Urimap = Map.Make (struct type t = uri let compare = compare end);;
module Uriset = Set.Make (struct type t = uri let compare = compare end);;

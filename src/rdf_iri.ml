(*********************************************************************************)
(*                OCaml-RDF                                                      *)
(*                                                                               *)
(*    Copyright (C) 2012-2014 Institut National de Recherche en Informatique     *)
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

exception Invalid_iri of string * string

let invalid_iri str msg = raise (Invalid_iri (str, msg));;

type iri = Iri.iri
type iri_reference = Iri.iri_reference

let iri ?normalize s =
  try Iri.of_string ?normalize s
  with Iri.Error e ->
    invalid_iri (s, Iri.string_of_error e)
;;

let string = Iri.to_string

let to_uri iri =
  let s = to_string ~encode: true (parse iri) in
  Rdf_uri.uri s
;;
let of_uri uri = iri (pct_decode (Rdf_uri.string uri));;

let concat i s = Iri.append_path i [s]

let compare = Iri.compare
let equal i1 i2 = compare i1 i2 = 0;;

module Iriset = Iri.Set
module Irimap = Iri.Map

let getter f iri = f (parse iri);;
let setter f iri x = to_string (f (parse iri) x);;

let scheme = Iri.scheme
let set_scheme = Iri.with_scheme

let user = Iri.user
let set_user = Iri.with_user

let host = Iri.host
let set_host = Iri.with_host

let port = Iri.port
let set_port = Iri.with_port

let path = Iri.path
let set_path = Iri.with_path

let query = Iri.query
let set_query = Iri.with_query

let fragment = Iri.fragment
let set_fragment = setter (fun i x -> { i with fragment = x });;


  
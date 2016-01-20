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

let s = "http://éxample.net/coucou";;
let iri = Iri.of_string s;;

let urn = "urn://uuid:2f302fb5-642e-4d3b-af19-29a8f6d894c9"
let iri_urn = Iri.of_string urn;;

let pct = "http://foo.bar/%66o%61%66.net";;
let iri = Iri.of_string pct ;;
print_endline (Iri.to_string iri);;

List.iter prerr_endline (Rdf_iri.path iri);;

let s2 = "Http://foo@example.net/ZazÀÖØöø˿Ͱͽ΄῾‌‍⁰↉Ⰰ⿕、ퟻ﨎ﷇﷰ￯";;
let iri2 = Iri.of_string s2;;
prerr_endline (Printf.sprintf "user(iri2)=%s" (Rdf_misc.string_of_opt (Rdf_iri.user iri2)));;

let uri2 = Rdf_iri.to_uri iri2;;
let iri3 = Rdf_iri.of_uri uri2;;

prerr_endline
  (Printf.sprintf "iri2=%s\nuri2=%s\niri3=%s"
    (Iri.to_string iri2) (Rdf_uri.string uri2) (Iri.to_string iri3));;

let s = "http://foo.org/with%20space";;
let uri = Rdf_uri.uri s;;
let iri = Rdf_iri.of_uri uri;;
prerr_endline (Printf.sprintf "s=%s\nuri=%s\niri=%s"
  s (Rdf_uri.string uri) (Iri.to_string iri));;


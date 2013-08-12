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

(** URIs. *)

(** URIs are abstract. {b Do not compare with generic comparison
  functions} ([Pervasives.compare], (=), ...) as it contains
  functional values. Use {!equal} or {!compare}. *)
type uri

exception Invalid_url of string

(** Create a string from a URI. *)
val string : uri -> string

(** Create a URI from a string.
     @raise Invalid_url in case the string does not represent a valid URL.
*)
val uri : string -> uri

(** Add the given string to the path of the given URI, using '/' as separator. *)
val concat : uri -> string -> uri

(** Return a new URI with the path modified to parent path of the original URI. *)
val parent : uri -> uri

(** Modify the fragment part of the URI. *)
val set_fragment : uri -> string -> uri

(** Get the path part of the URI. *)
val path : uri -> string list

(** Comparison of two URIs, as usual. *)
val compare : uri -> uri -> int

(** Equality over URIs. *)
val equal : uri -> uri -> bool

(** Get a {!Neturl.url} from the given URI. ([Neturl.url]
  is the underlying represention of URIs). *)
val neturl : uri -> Neturl.url

(** Get a {!uri} from the given {!Neturl.url}. *)
val of_neturl : Neturl.url -> uri

module Urimap : Map.S with type key = uri
module Uriset : Set.S with type elt = uri

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

(** IRIs. See {{:http://www.ietf.org/rfc/rfc3987.txt}RFC3987}. *)


(** IRIs are abstract. {b Do not compare with generic comparison
  functions} ([Pervasives.compare], (=), ...). Use {!equal} or {!compare}. *)
type iri

(** When the IRI is parsed,
  [Invalid_iri (string, message)] is raised if the
  given string is invalid, with an additional message. *)
exception Invalid_iri of string * string

(** Create a string from an IRI. *)
val string : iri -> string

(** Create an IRI from a string.
   @raise Invalid_iri in case the string does not represent a valid IRI.
   @param check can be specified to [false] to prevent IRI parsing.
   Parsing will be done later if needed, and may then raise [Invalid_iri].
*)
val iri : ?check: bool -> string -> iri

(** [ensure_absolute base s] creates an IRI from [s], using the given [base]
 if [s] is relative. Apply {{:http://tools.ietf.org/html/rfc3986#section-5.4}these rules}. *)
val ensure_absolute : iri -> string -> iri

(** Append the given string to the given IRI.
     @param check can be specified to [false] to prevent IRI parsing.
*)
val append : ?check: bool -> iri -> string -> iri

(** Add the given string to the path of the given IRI.*)
val concat : iri -> string -> iri

(** [to_uri iri] encodes the given IRI and tries to create an URI.
  May raise [Rdf_uri.Invalid_uri] is the IRI cannot be mapped to
  an URI. *)
val to_uri : iri -> Rdf_uri.uri

(** [of_uri uri] get the string representation of the given [uri],
  then percent-decode it and build an iri from this decoded string,
  using {!iri}. *)
val of_uri : Rdf_uri.uri -> iri

val equal : iri -> iri -> bool

val compare : iri -> iri -> int

module Irimap : Map.S with type key = iri
module Iriset : Set.S with type elt = iri

val scheme : iri -> string
val set_scheme : iri -> string -> iri
val user : iri -> string option
val set_user : iri -> string option -> iri
val host : iri -> string option
val set_host : iri -> string option -> iri
val port : iri -> int option
val set_port : iri -> int option -> iri
val path : iri -> string list
val set_path : iri -> string list -> iri
val query : iri -> string option
val set_query : iri -> string option -> iri
val fragment : iri -> string option
val set_fragment : iri -> string option -> iri

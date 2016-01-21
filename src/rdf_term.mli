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

(** RDF terms. *)

(** Literal terms contain a value, an optional language and an optional data type IRI. *)
type literal = {
  lit_value : string;
  lit_language : string option;
  lit_type : Iri.t option;
}

(** Type for blank node ids. *)
type blank_id

(** Various kinds of terms. *)
type term =
  | Iri of Iri.t
  | Literal of literal
  | Blank
  | Blank_ of blank_id

val compare : term -> term -> int

module Ordered_term : sig type t = term val compare : term -> term -> int end
module TSet : Set.S with type elt = term
module TMap : Map.S with type key = term

(** A RDF triple is triple (term, iri, term). *)
type triple = term * Iri.t * term

(** Get a string from a blank term id. *)
val string_of_blank_id : blank_id -> string

(** Make a blank term id from a string. *)
val blank_id_of_string : string -> blank_id

(** Shortcut for [Iri (Iri.of_string string)]. *)
val term_of_iri_string : string -> term

(** Creation of a literal. *)
val mk_literal : ?typ:Iri.t -> ?lang:string -> string -> literal

(** Create a datetime literal with type iri from the given datetime [d].
  If no date is given, [Unix.time()] is used.*)
val mk_literal_datetime : ?d:CalendarLib.Fcalendar.t -> unit -> literal

(** Create a literal term from the given datetime. (see {!mk_literal_datetime}). *)
val term_of_datetime : ?d:CalendarLib.Fcalendar.t -> unit -> term

(** Parse a string to get a datetime. *)
val datetime_of_string : string -> CalendarLib.Fcalendar.t

(** Parse a literal to get a datetime. *)
val datetime_of_literal : literal -> CalendarLib.Fcalendar.t

(** Create a boolean literal with type iri from the given boolean. *)
val mk_literal_bool : bool -> literal

(** Create an integer literal. *)
val mk_literal_int : int -> literal

(** Create a double literal. *)
val mk_literal_double : float -> literal

(** Parse a literal to get a boolean. *)
val bool_of_literal : literal -> bool

(** Shortcut for [Literal (mk_literal ?typ ?lang string)] *)
val term_of_literal_string : ?typ:Iri.t -> ?lang:string -> string -> term

(** Shortcut for [Literal (mk_literal ~typ: Rdf_rdf.xsd_integer int)] *)
val term_of_int : int -> term

(** Shortcut for [Literal (mk_literal ~typ: Rdf_rdf.xsd_double float)] *)
val term_of_double : float -> term

(** Create a literal term from the given boolean. (see {!mk_literal_bool}). *)
val term_of_bool : bool -> term

(** [quote_str str] returns the given string [str] between quotes, with
  correct literal string escapes. *)
val quote_str : string -> string

(** Create a string from the given RDF literal, using turtle syntax. *)
val string_of_literal : literal -> string

(** Create a string for the given term, using RDF turtle syntax conventions.
  @see <http://www.w3.org/TeamSubmission/turtle/#language> the description of turtle language. *)
val string_of_term : term -> string

(** [term_hash term] returns an int64 identifiying (hopefully unically)) a given term. *)
val term_hash : term -> int64

(** Literal "true" with according type. *)
val lit_true : literal

(** Literal "false" with according type. *)
val lit_false : literal

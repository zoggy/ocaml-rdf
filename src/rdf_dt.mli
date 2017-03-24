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

(** Computing values in Sparql, mapping to/from RDF terms.

   A value can be an error, and propagated in computations.
*)

type error =
  | Type_error of value * string (** The value has not the given expected "type" *)
  | Invalid_literal of Rdf_term.literal (** Invalid literal: bad integer string for an integer, ...*)
  | Exception of exn (** To keep the original exception which resulted in an error. *)

and value =
    Err of error (** Value is an error *)
  | Blank of string (** A blank node with its label *)
  | Iri of Iri.t (** An IRI. *)
  | String of string (** A string literal. *)
  | Int of int * Iri.t  (** An integer and the original datatype IRI *)
  | Float of float  (** A decimal, float or double. *)
  | Bool of bool  (** A Boolean. *)
  | HexBinary of string (** Binary data in hexadecimal, in lowercase *)
  | Datetime of CalendarLib.Fcalendar.t (** A datetime. *)
  | Ltrl of string * string option  (** A literal string with an optional language tag. *)
  | Ltrdt of string * Iri.t  (** A literal value with a specified datatype. *)

exception Error of error

(** Raise a {!Error} exception with the given error. *)
val error : error -> 'a

(** Default date format. *)
val date_fmt : string

(** Return a string to show the given value. *)
val string_of_value : value -> string

module ValueOrdered :
  sig type t = value val compare : value -> value -> int end

(** Maps over values. *)
module VMap : Map.S with type key = value

(** Sets of values. *)
module VSet : Set.S with type elt = value

(** Return a human-readable message from the given error. *)
val string_of_error : error -> string

(** [iri base v] returns a [Iri] value, ensure the given value is
  an IRI. If it is a literal string, convert it to an IRI, eventually
  appending to the [base] IRI if the string expresses a relative IRI. *)
val iri : Iri.t -> value -> value

(** [datatype v] returns the IRI of the datatype of the value.
  If v is [Err], [Blank] or [Iri], return [Err].*)
val datatype : value -> value


(** [string_literal v] returns a pair (string, optional language tag) if
  [v] is [String] of [Ltrl]. Else return [Err]. *)
val string_literal : value -> string * string option

(** [string v] returns a [String] value, converting any value
  to a string, except [Err] and [Blank] for which [Err] is returned. *)
val string : value -> value

(** [int v] returns a [Int] value, trying to convert literal values
  to an integer. Return [Err] if [v] is [Err], [Blank] or [Iri], or
  if the literal value could not be converted to an integer. Floats
  are truncated. *)
val int : value -> value

(** Same as {!int} but for floats. *)
val float : value -> value

(** Same as {!int} but for booleans.
  [Bool true], [String "true"], [String "1"], [Int n] with [n<>0] and [Float f] when [f] is not nan nor zero evaluate
  to [Bool true].
  [Bool false], [String "false"], [String "0"], [Int 0] and [Float f] when [f] is [nan] or zero evaluate to
  [Bool false].
  Any other value evaluates to [Err].
*)
val bool : value -> value

(** [datetime v] returns a [Datetime], if possible. String literals are converted if
  possible; else [Err] is returned. *)
val datetime : value -> value

(** Same as {!string}, but languge tag is kept is present (i.e. if
  the value is a [Ltrl]).*)
val ltrl : value -> value

(** Try to convert the given value to an [Int] or else to a [Float],
  if the value is not already [Int] or [Float]. *)
val numeric : value -> value

(** [of_literal lit] returns a value from a literal term. *)
val of_literal : Rdf_term.literal -> value

(** [of_term term] returns a value from an RDF term. *)
val of_term : Rdf_term.term -> value

(** [to_term v] converts the given value to an RDF term.
  If [v] is [Error e], then exception [e] is raised.*)
val to_term : value -> Rdf_term.term

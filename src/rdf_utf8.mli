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

(** Handling UTF-8 strings. *)

(** Return the number of string taken by a the first byte
  (as character) of a UTF-8 character. *)
val utf8_nb_bytes_of_char : char -> int

(** [utf8_index_of_char str n] returns the position in byte
  of the [n]th character. [n] is 0-based.
  @raise Not_found if there is no [n]th character.*)
val utf8_index_of_char : string -> int -> int

(** [utf8_char_of_index str n] returns the utf8 character
  position corresponding to the given byte index [n].*)
val utf8_char_of_index : string -> int -> int

(** [utf8_string_length str] returns the number of utf8
  characters in [str]. *)
val utf8_length : string -> int

(** [utf8_substr str pos l] returns the substring of [str]
  from utf8 position [pos] to [pos+l-1]. *)
val utf8_substr : string -> int -> int -> string

(** [utf8_is_prefix s1 s2] returns whether [s2] is prefix of [s1].*)
val utf8_is_prefix : string -> string -> bool

(** [utf8_is_suffix s1 s2] returns whether [s2] is suffix of [s1].*)
val utf8_is_suffix : string -> string -> bool

(** [utf8_substr_pos s1 s2] returns [Some n] if [s2] is found
  at position [n] in [s1], starting to search from the beginning
  of [s1]. Else return [None]. *)
val utf8_substr_pos : string -> string -> int option

(** [utf8_contains s1 s2] returns whether [s1] contains [s2]. *)
val utf8_contains : string -> string -> bool

(** [utf8_strbefore s1 s2] returns the substring before [s2] in [s1].
  See {{:http://www.w3.org/TR/sparql11-query/#func-strbefore}details}.
*)
val utf8_strbefore : string -> string -> string

(** [utf8_strafter s1 s2] returns the substring after [s2] in [s1].
  See {{:http://www.w3.org/TR/sparql11-query/#func-strafter}details}.
*)
val utf8_strafter : string -> string -> string

(** [utf8_char_of_code n] return the UTF8 character from a given codepoint. *)
val utf8_char_of_code : int32 -> string

(** [utf8_get_bol str] returns the list of pairs
  [(line number, char position)] giving the position of beginning of each line
  in the given string. *)
val utf8_get_bol : string -> (int * int) list

(** [utf8_count_nl str] returns the number of newline \n characters found
  in the given UTF-8 string. *)
val utf8_count_nl : string -> int

(** [utf8_escape str] \-escapes the following characters : \n, \r, \b, \t, \f,
  \(quotes), \' and \\ but does not escape \u nor \U. *)
val utf8_escape : string -> string

(** [utf8_unescape str] unescape the following escaped characters:
  \n, \r, \b, \t, \f, \quotes, \' and \\, and also \u... and \U... sequences . *)
val utf8_unescape : string -> string

(** [utf8_lowercase s] returns a new string, with some characters
  mapped to corresponding lowercase characters.
  @warning By now, it only applies OCaml's [Char.lowercase] on one byte long characters.
*)
val utf8_lowercase : string -> string

(** [utf8_uppercase s] returns a new string, with some characters
  mapped to corresponding uppercase characters.
  @warning By now, it only applies OCaml's [Char.uppercase] on one byte long characters.
*)
val utf8_uppercase : string -> string

(** [utf8_backslash_quotes s] returns a new string like [s]
  with a backslash before each double quotes.*)
val utf8_backslash_quotes : string -> string


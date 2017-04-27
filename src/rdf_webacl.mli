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

(** Convenient stuff to handle webacls *)

type rights

val no_right : rights
val add_read : rights -> rights
val rem_read : rights -> rights
val has_read : rights -> bool
val add_write : rights -> rights
val rem_write : rights -> rights
val has_write : rights -> bool
val add_append : rights -> rights
val rem_append : rights -> rights
val has_append : rights -> bool
val add_control : rights -> rights
val rem_control : rights -> rights
val has_control : rights -> bool

val all_rights : rights

val add_rights_of_modes : rights -> Iri.t list -> rights
val modes_of_rights : rights -> Iri.t list

val rights_to_string : rights -> string

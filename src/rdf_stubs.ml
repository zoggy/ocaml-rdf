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

(** *)

exception Not_implemented of string
let not_impl str = raise (Not_implemented str)

let sha1 : (string -> string) ref = ref (fun _ -> not_impl "sha1")
let sha256 : (string -> string) ref = ref (fun _ -> not_impl "sha256")

let pcre_replace :
  (flags: Pcre.cflag list -> pat: string -> templ:string -> string -> string) ref =
  ref (fun ~flags ~pat ~templ str -> not_impl "pcre_replace")

let pcre_pmatch :
  (flags: Pcre.cflag list -> pat: string -> string -> bool) ref =
  ref (fun ~flags ~pat str -> not_impl "pcre_pmath")
  
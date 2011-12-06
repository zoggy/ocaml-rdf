(*********************************************************************************)
(*                OCaml-RDF                                                      *)
(*                                                                               *)
(*    Copyright (C) 2011 Institut National de Recherche en Informatique          *)
(*    et en Automatique. All rights reserved.                                    *)
(*                                                                               *)
(*    This program is free software; you can redistribute it and/or modify       *)
(*    it under the terms of the GNU Lesser General Public License version        *)
(*    2.1 or later as published by the Free Software Foundation.                 *)
(*                                                                               *)
(*    This program is distributed in the hope that it will be useful,            *)
(*    but WITHOUT ANY WARRANTY; without even the implied warranty of             *)
(*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the              *)
(*    GNU Library General Public License for more details.                       *)
(*                                                                               *)
(*    You should have received a copy of the GNU Lesser General Public           *)
(*    License along with this program; if not, write to the Free Software        *)
(*    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA                   *)
(*    02111-1307  USA                                                            *)
(*                                                                               *)
(*    Contact: Maxence.Guesdon@inria.fr                                          *)
(*                                                                               *)
(*                                                                               *)
(*********************************************************************************)

(** RDF syntax library.
  Interface to Raptor2.

  @rdfmod index.html
*)

open Rdf_types;;

(**/**)
module Raw =
  struct
    external new_world : unit -> raptor_world option = "ml_raptor_new_world"
    external free_world : raptor_world -> unit = "ml_raptor_free_world"
    external world_open : raptor_world -> int = "ml_raptor_world_open"


    external free_iostream : raptor_iostream -> unit = "ml_raptor_free_iostream"
    external new_iostream_to_file_handle : raptor_world -> Unix.file_descr -> raptor_iostream option =
      "ml_raptor_new_iostream_to_file_handle"

    external pointer_of_world : raptor_world -> Nativeint.t = "ml_pointer_of_custom"
  end

let world_to_finalise v = () (*Gc.finalise Raw.free_world v;;*)
let iostream_to_finalise v = Gc.finalise Raw.free_iostream v;;
(**/**)

exception Raptor_world_creation_failed of string;;
exception Raptor_iostream_failed of string;;

(** {2 World}
  {rdfmod raptor2-section-world.html}
  {rdfprefix raptor_}
*)

let on_new_world fun_name = function
  None -> raise (Raptor_world_creation_failed fun_name)
| Some n -> world_to_finalise n; n
;;

(** @rdf new_world *)
let new_world () = on_new_world "" (Raw.new_world ());;

(** {2 I/O streams}
  {rdfmod raptor2-section-iostream.html}
*)

let on_new_iostream fun_name = function
  None -> raise (Raptor_iostream_failed fun_name)
| Some n -> iostream_to_finalise n; n
;;

(** @rdf new_iostream_to_file_handle *)
let new_iostream_to_file_handle w fh =
  on_new_iostream "to_file_handle"
  (Raw.new_iostream_to_file_handle w fh)
;;

(** {rdfprefix raptor_iostream_} *)

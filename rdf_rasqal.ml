(*********************************************************************************)
(*                OCaml-RDF                                                      *)
(*                                                                               *)
(*    Copyright (C) 2011 Institut National de Recherche en Informatique          *)
(*    et en Automatique. All rights reserved.                                    *)
(*                                                                               *)
(*    This program is free software; you can redistribute it and/or modify       *)
(*    it under the terms of the GNU Library General Public License version       *)
(*    2.1 or later as published by the Free Software Foundation.                 *)
(*                                                                               *)
(*    This program is distributed in the hope that it will be useful,            *)
(*    but WITHOUT ANY WARRANTY; without even the implied warranty of             *)
(*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the              *)
(*    GNU Library General Public License for more details.                       *)
(*                                                                               *)
(*    You should have received a copy of the GNU Library General Public          *)
(*    License along with this program; if not, write to the Free Software        *)
(*    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA                   *)
(*    02111-1307  USA                                                            *)
(*                                                                               *)
(*    Contact: Maxence.Guesdon@inria.fr                                          *)
(*                                                                               *)
(*                                                                               *)
(*********************************************************************************)

(** RDF query library.
  Interface to Raptor2.

  @rdfmod index.html
*)


open Rdf_types;;

(**/**)
module Raw =
  struct
    external new_world : unit -> rasqal_world option = "ml_rasqal_new_world"
    external free_world : rasqal_world -> unit = "ml_rasqal_free_world"
    external world_open : rasqal_world -> int = "ml_rasqal_world_open"
    external world_set_raptor : rasqal_world -> raptor_world option -> unit = "ml_rasqal_world_set_raptor"
    external world_get_raptor : rasqal_world -> raptor_world option = "ml_rasqal_world_get_raptor"
    external pointer_of_world : rasqal_world -> Nativeint.t = "ml_pointer_of_custom"
  end

let world_to_finalise v =();;(* Gc.finalise Raw.free_world v;;*)
(**/**)

exception Rasqal_world_creation_failed of string;;

(** {2 General}
 {rdfmod rasqal-section-general.html}
 {rdfprefix rasqal_}
*)

let on_new_world fun_name = function
  None -> raise (Rasqal_world_creation_failed fun_name)
| Some n -> world_to_finalise n; n
;;

(** @rdf new_world *)
let new_world () = on_new_world "" (Raw.new_world ());;

(** @rdf world_open *)
let world_open = Raw.world_open;;

(** @rdf world_set_raptor *)
let world_set_raptor = Raw.world_set_raptor;;

(** @rdf world_get_raptor *)
let world_get_raptor = Raw.world_get_raptor;;

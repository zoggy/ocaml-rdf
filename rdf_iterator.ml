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

(** Iterators.
  @rdfmod redland-iterator.html
  @rdfprefix librdf_
*)

open Rdf_types;;

(**/**)
let dbg = Rdf_misc.create_log_fun ~prefix: "Rdf_iterator" "ORDF_ITERATOR_DEBUG_LEVEL";;

module Raw =
  struct
    external free : 'a iterator -> unit = "ml_librdf_free_iterator"
    external is_at_end : 'a iterator -> bool = "ml_librdf_iterator_end"
    external next : 'a iterator -> bool = "ml_librdf_iterator_next"
    external get_object : 'a iterator -> 'a option = "ml_librdf_iterator_get_object"
    external get_context : 'a iterator -> 'b option = "ml_librdf_iterator_get_context"
(*
    external get_key : 'a iterator -> 'a option = "ml_librdf_iterator_get_key"
    external get_value : 'a iterator -> 'a option = "ml_librdf_iterator_get_value"
*)
    external pointer_of_iterator : 'a iterator -> Nativeint.t = "ml_pointer_of_custom"
   end
let free v =
  dbg (fun () -> Printf.sprintf "Freeing iterator %s"
   (Nativeint.to_string (Raw.pointer_of_iterator v)));
  Raw.free v
;;
let to_finalise v = Gc.finalise free v;;
(**/**)

exception Iterator_creation_failed of string;;

let on_new_iterator fun_name = function
  None -> raise (Iterator_creation_failed fun_name)
| Some n -> to_finalise n; n
;;

(** @rdf iterator_end *)
let is_at_end = Raw.is_at_end ;;

(** @rdf iterator_next *)
let next = Raw.next ;;

(** @rdf iterator_get_object *)
let get_object it copy =
  Rdf_misc.map_opt copy (Raw.get_object it);;

(** @rdf iterator_get_context *)
let get_context ?(copy=(fun x -> x)) it =
  Rdf_misc.map_opt copy (Raw.get_context it)
;;

let fold_objects iterator copy f =
  let rec iter acc =
    if is_at_end iterator then
       acc
    else
      (
        let acc =
        match get_object iterator copy with
          None -> acc
        | Some o -> f acc o
        in
        ignore(next iterator) ;
        iter acc
      )
  in
  iter []
;;



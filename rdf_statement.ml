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

(** RDF triple (Statement).
  @rdfmod redland-statement.html
  @rdfprefix librdf_
*)

open Rdf_types;;

(**/**)
let dbg = Rdf_misc.create_log_fun ~prefix: "Rdf_statement" "ORDF_STATEMENT";;

module Raw =
  struct
    external new_statement : world -> statement option = "ml_librdf_new_statement"
    external free : statement -> unit = "ml_librdf_free_statement"
    external new_from_statement : statement -> statement option = "ml_librdf_new_statement_from_statement"

    external new_from_nodes :
      world -> node -> node -> node -> statement option = "ml_librdf_new_statement_from_nodes"

    external init : world -> statement -> unit = "ml_librdf_statement_init"
    external clear : statement -> unit = "ml_librdf_statement_clear"

    external get_subject : statement -> node = "ml_librdf_statement_get_subject"
    external set_subject : statement -> node -> unit = "ml_librdf_statement_set_subject"

    external get_predicate : statement -> node = "ml_librdf_statement_get_predicate"
    external set_predicate : statement -> node -> unit = "ml_librdf_statement_set_predicate"

    external get_object : statement -> node = "ml_librdf_statement_get_object"
    external set_object : statement -> node -> unit = "ml_librdf_statement_set_object"

    external is_complete : statement -> bool = "ml_librdf_statement_is_complete"
    external equals : statement -> statement -> bool = "ml_librdf_statement_equals"
    external matches : statement -> statement -> bool = "ml_librdf_statement_match"

    external print : statement -> Unix.file_descr -> unit =
      "ml_librdf_statement_print"

    external pointer_of_statement : statement -> Nativeint.t = "ml_pointer_of_custom"
end;;

let free v =
  dbg (fun () -> Printf.sprintf "Freeing statement %s"
   (Nativeint.to_string (Raw.pointer_of_statement v)));
  Raw.free v
;;
let to_finalise v = Gc.finalise free v;;
(**/**)

exception Statement_creation_failed of string;;

let on_new_statement fun_name = function
  None -> raise (Statement_creation_failed fun_name)
| Some n -> to_finalise n; n
;;

(** @rdf new_statement *)
let new_statement world =
  on_new_statement "" (Raw.new_statement world)
;;

(** @rdf new_statement_from_statement *)
let copy_statement st =
  on_new_statement "from_statement" (Raw.new_from_statement st)
;;

(** @rdf new_statement_from_nodes *)
let new_from_nodes world ~sub ~pred ~obj =
  on_new_statement "from_nodes"
  (Raw.new_from_nodes world
    (Rdf_node.copy_node sub)
    (Rdf_node.copy_node pred)
    (Rdf_node.copy_node obj)
  )
;;

(** @rdf statement_get_subject *)
let get_subject st =
  Rdf_node.copy_node (Raw.get_subject st)
;;

(** @rdf statement_set_subject *)
let set_subject st n =
  Raw.set_subject st (Rdf_node.copy_node n)
;;

(** @rdf statement_get_predicate *)
let get_predicate st =
  Rdf_node.copy_node (Raw.get_predicate st)
;;

(** @rdf statement_set_predicate *)
let set_predicate st n =
  Raw.set_predicate st (Rdf_node.copy_node n)
;;

(** @rdf statement_get_object *)
let get_object st =
  Rdf_node.copy_node (Raw.get_object st)
;;

(** @rdf statement_set_object *)
let set_object st n =
  Raw.set_object st (Rdf_node.copy_node n)
;;

(** @rdf statement_is_complete *)
let is_complete = Raw.is_complete

(** @rdf statement_equals *)
let equals = Raw.equals

(** @rdf statement_match *)
let matches = Raw.matches

(** @rdf statement_print *)
let print statement fd =
  Raw.print statement fd;;


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

(** Queries.
  @rdfmod redland-query.html
  @rdfprefix librdf_
*)

open Rdf_types;;

(**/**)
let dbg = Rdf_misc.create_log_fun ~prefix: "Rdf_query" "ORDF_QUERY_DEBUG_LEVEL";;

module Raw =
  struct
    external new_query : world ->
      string -> uri option -> string -> uri option -> query option = "ml_librdf_new_query"

    external new_from_query :
      query -> query option = "ml_librdf_new_query_from_query"
    external free : query -> unit = "ml_librdf_free_query"

    external pointer_of_query : query -> Nativeint.t = "ml_pointer_of_custom"
   end

let free v =
  dbg (fun () -> Printf.sprintf "Freeing query %s"
   (Nativeint.to_string (Raw.pointer_of_query v)));
  Raw.free v
;;
let to_finalise v = Gc.finalise free v;;
(**/**)

exception Query_creation_failed of string;;

let on_new_query fun_name = function
  None -> raise (Query_creation_failed fun_name)
| Some n -> to_finalise n; n
;;

(** @rdf new_query *)
let new_query ~name ?uri ?base ~query world =
  on_new_query "" (Raw.new_query world name uri query base)
;;

(** @rdf new_query_from_query *)
let copy_query query =
  on_new_query "from_query" (Raw.new_from_query query)
;;

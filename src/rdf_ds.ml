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

(** *)

module Irimap = Iri.Map
module Iriset = Iri.Set

exception Could_not_retrieve_graph of Iri.t * string
let could_not_retrieve_graph iri msg =
  raise (Could_not_retrieve_graph (iri, msg))
;;

type dataset =
  { default : Rdf_graph.graph ;
    named : Iriset.t ;
    get_named : Iri.t -> Rdf_graph.graph ;
  }

let simple_dataset ?(named=[]) default =
  let named_set = List.fold_left (fun set (iri,_) -> Iriset.add iri set) Iriset.empty named in
  let named = List.fold_left (fun map (iri,g) -> Irimap.add iri g map) Irimap.empty named in
  let get_named iri =
    try Irimap.find iri named
    with Not_found ->
        could_not_retrieve_graph iri
          ("Unknown graph "^(Iri.to_string iri))
  in
  { default ; named = named_set ; get_named }
;;

let dataset ?get_named ?(named=Iriset.empty) default =
  match get_named with
    None -> simple_dataset default
  | Some get_named -> { default ; named ; get_named }
;;

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

module C = Config_file
val verb : string -> unit
type test_spec = {
  base : Iri.t option;
  title : string;
  desc : string option;
  query : string;
  default_graph : string option;
  named : (Iri.t * string) list;
  options : (string * string) list;
}
type result = Error of string | Ok of Rdf_sparql.query_result
type test = { spec : test_spec; result : result; }
val load_file : ?graph_options:string -> string -> test_spec
val load_ttl : Rdf_graph.graph -> Iri.t -> string -> unit
val mk_graph : test_spec -> Rdf_graph.graph
val mk_dataset : test_spec -> Rdf_ds.dataset
val print_solution : Rdf_sparql.solution -> unit
val print_result : result -> unit

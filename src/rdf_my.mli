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

(** Mysql storage common parts. *)

val dbg : ?loc:string -> ?level:int -> (unit -> string) -> unit
type t = {
  g_name : Iri.t;
  g_table : string;
  g_dbd : Mysql.dbd;
  mutable g_in_transaction : bool;
  g_transactions : bool;
}
type error = string
exception Error of string
val string_of_error : 'a -> 'a
module SMap = Rdf_sparql_types.SMap
val exec_query : Mysql.dbd -> string -> Mysql.result
val exec_prepared : Mysql.dbd -> string -> string list -> Mysql.result
val db_of_options : Rdf_graph.options -> Mysql.db
val mysql_quote_dbd : Mysql.dbd -> string -> string
val mysql_quote : t -> string -> string
val init_db : Mysql.db -> string list -> Mysql.dbd
val graph_table_of_id : int -> string
val graph_table_of_graph_name : ?first:bool -> Mysql.dbd -> Iri.t -> string
val nstable_of_graph_table : string -> string
val table_exists : Mysql.dbd -> string -> bool
val create_namespaces_table : Mysql.dbd -> string -> unit
val prepared_count_triples : string
val prepared_insert_triple : string
val prepared_delete_triple : string
val prepared_subjects_of : string
val prepared_predicates_of : string
val prepared_objects_of : string
val prepared_subject : string
val prepared_predicate : string
val prepared_object : string
val prepared_cardinal : string
val prepared_namespaces : string
val prepared_delete_namespace : string
val prepared_insert_namespace : string
val make_select_term_list : string -> string -> string -> string
val prepare_query : Mysql.dbd -> string -> string -> unit
val prepare_queries :
  Mysql.dbd -> ?more:(string * string) list -> string -> unit
val namespaces : t -> (Iri.t * string) list
val rem_namespace : t -> string -> unit
val add_namespace : t -> Iri.t -> string -> unit
val set_namespaces : t -> (Iri.t * string) list -> unit
val graph_size : t -> int
val exists :
  (?sub:'a -> ?pred:'b -> ?obj:'c -> t -> string) ->
  ?sub:'a -> ?pred:'b -> ?obj:'c -> t -> bool
val transaction_start : t -> unit
val transaction_commit : t -> unit
val transaction_rollback : t -> unit
val new_blank_id : t -> Rdf_term.blank_id

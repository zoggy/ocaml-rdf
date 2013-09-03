(*********************************************************************************)
(*                OCaml-RDF                                                      *)
(*                                                                               *)
(*    Copyright (C) 2012-2013 Institut National de Recherche en Informatique     *)
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

(** Graph abstraction.

  The graph provides an abstraction of the storage used (memory, database, ...).
  The graph is modified in place.

  Example of usage:
   {[
let options =
  [
    "storage", "mysql" ;
    "database", "mydb";
    "user", "john" ;
  ]
in
let graph = Rdf_graph.open_graph ~options (Rdf_uri.uri "http://hello.fr") in
graph.add_triple
  ~sub: (Rdf_term.term_of_uri_string "http://john.net")
  ~pred: (Rdf_uri.uri "http://relations.org/hasMailbox")
  ~obj: (Rdf_term.term_of_literal_string "john\@john.net");
...
]}
*)

(** {2 Options} *)

type options = (string * string) list

(** [get_options name options] returns the value associated to the
  option with the given name, in option list.
  If the option name is not found in the list, the function raises
  the [Failure] exception with a message about the missing option.
  @param def can be used to specify a default value; in this case, if
  the option name was not found in list, the default value is
  returned instead of raising [Failure].
*)
val get_option : ?def:string -> string -> options -> string

(** {2 Creating storages}

This is useful only to create your own storage. *)

(** Interface to query Basic Graph Patterns (BGP) in a graph. *)
module type Storage_BGP =
  sig
    type g
    type term
    val term : g -> Rdf_term.term -> term
    val compare : g -> term -> term -> int
    val rdfterm : g -> term -> Rdf_term.term
    val subjects : g -> term list
    val objects : g -> term list
    val find :
        ?sub:term ->
        ?pred:term ->
        ?obj:term -> g -> (term * term * term) list
  end
;;

(** A storage is a module with this interface. *)
module type Storage =
  sig
    (** The name of the storage, for example "mysql". *)
    val name : string

    (** The type of the graph, abstract. It usually includes
      all information needed by the other functions, as various
      graphs of the same kind can be used in the same application.*)
    type g

    (** {3 Errors} *)

    (** A specific type for errors.*)
    type error

    (** This is the exception raised by the functions of the module
      in case of error. *)
    exception Error of error

    (** This function returns a message from the given error. *)
    val string_of_error : error -> string

    (** {3 Creation and modification} *)

    (** Creationg of the graph. The graph has a name which is a URI. *)
    val open_graph : ?options:(string * string) list -> Rdf_uri.uri -> g

    (** Access to the graph name, as specified at its creation. *)
    val graph_name : g -> Rdf_uri.uri

    (** Return the number of triples in the graph. *)
    val graph_size : g -> int

    (** Adding a triple to the graph. *)
    val add_triple :
      g ->
      sub:Rdf_term.term -> pred:Rdf_uri.uri -> obj:Rdf_term.term -> unit

    (** Removing a triple from the graph. *)
    val rem_triple :
      g ->
      sub:Rdf_term.term -> pred:Rdf_uri.uri -> obj:Rdf_term.term -> unit

    (** Adding a triple to the graph, curryfied form. *)
    val add_triple_t : g -> Rdf_term.triple -> unit

    (** Removing a triple from the graph, curryfied form. *)
    val rem_triple_t : g -> Rdf_term.triple -> unit

    (** {3 Querying the graph} *)

    (** [subjects_of g ~pred ~obj] returns the list of nodes which are
      subjects in triples with the specified predicate and object. *)
    val subjects_of :
      g -> pred:Rdf_uri.uri -> obj:Rdf_term.term -> Rdf_term.term list

    (** [predicates_of g ~sub ~obj] returns the list of nodes which are
      predicates in triples with the specified subject and object. *)
    val predicates_of :
      g -> sub:Rdf_term.term -> obj:Rdf_term.term -> Rdf_uri.uri list

    (** [objects_of g ~sub ~pred] returns the list of nodes which are
      objects in triples with the specified subject and predicate. *)
    val objects_of :
      g -> sub:Rdf_term.term -> pred:Rdf_uri.uri -> Rdf_term.term list

    (** [find ?sub ?pred ?obj g] returns the list of triples matching the
         constraints given by the optional subject, predicate and object.
         One can specify, zero, one, two or three of these nodes. *)
    val find :
      ?sub:Rdf_term.term ->
      ?pred:Rdf_uri.uri -> ?obj:Rdf_term.term -> g -> Rdf_term.triple list

    (** Same as {!find} but only returns [true] if at least one triple
      of the graph matches the constraints. *)
    val exists :
      ?sub:Rdf_term.term ->
      ?pred:Rdf_uri.uri -> ?obj:Rdf_term.term -> g -> bool

    (** Curryfied version of {!exists}. *)
    val exists_t : Rdf_term.triple -> g -> bool

    (** Return the list of nodes appearing in subject position. *)
    val subjects : g -> Rdf_term.term list

    (** Return the list of nodes appearing in predicate position. *)
    val predicates : g -> Rdf_uri.uri list

    (** Return the list of nodes appearing in object position. *)
    val objects : g -> Rdf_term.term list

    (** {3 Transactions} *)

    (** Start a transaction. All storage may not support transactions.*)
    val transaction_start : g -> unit

    (** Commit. *)
    val transaction_commit : g -> unit

    (** Rollback. *)
    val transaction_rollback : g -> unit

    (** Forging a new, unique blank node id. *)
    val new_blank_id : g -> Rdf_term.blank_id

    module BGP : Storage_BGP with type g = g
  end

(** This is the exception raised by the module we get when applying
  {!Make} on a storage.

  Each call to a {!Storage} function is embedded so that the
  {!Storage_error} exception is raised when an error occurs in
  a storage function.
  The exception provides the name of the storage, the error message
  (obtained with {!Storage.string_of_error}) and the original exception.

  Refer to the documentation of {!Storage} for information about
  the functions provided by the resulting module.
*)
exception Storage_error of string * string * exn

module type Graph =
  sig
    type g
    val open_graph : ?options:(string * string) list -> Rdf_uri.uri -> g
    val graph_name : g -> Rdf_uri.uri
    val graph_size : g -> int
    val add_triple :
      g ->
      sub:Rdf_term.term -> pred:Rdf_uri.uri -> obj:Rdf_term.term -> unit
    val rem_triple :
      g ->
      sub:Rdf_term.term -> pred:Rdf_uri.uri -> obj:Rdf_term.term -> unit
    val add_triple_t : g -> Rdf_term.triple -> unit
    val rem_triple_t : g -> Rdf_term.triple -> unit
    val subjects_of :
      g -> pred:Rdf_uri.uri -> obj:Rdf_term.term -> Rdf_term.term list
    val predicates_of :
      g -> sub:Rdf_term.term -> obj:Rdf_term.term -> Rdf_uri.uri list
    val objects_of :
      g -> sub:Rdf_term.term -> pred:Rdf_uri.uri -> Rdf_term.term list
    val find :
      ?sub:Rdf_term.term ->
      ?pred:Rdf_uri.uri -> ?obj:Rdf_term.term -> g -> Rdf_term.triple list
    val exists :
      ?sub:Rdf_term.term ->
      ?pred:Rdf_uri.uri -> ?obj:Rdf_term.term -> g -> bool
    val exists_t : Rdf_term.triple -> g -> bool
    val subjects : g -> Rdf_term.term list
    val predicates : g -> Rdf_uri.uri list
    val objects : g -> Rdf_term.term list
    val transaction_start : g -> unit
    val transaction_commit : g -> unit
    val transaction_rollback : g -> unit
    val new_blank_id : g -> Rdf_term.blank_id

    module BGP : Storage_BGP with type g = g
  end
module Make : functor (S : Storage) -> Graph with type g = S.g

(** {2 Registering storages} *)

(** Add a storage to the list of registered storages. *)
val add_storage : (module Storage) -> unit

(** This is the structure returned by {!open_graph}. It contains
  the same functions as in {!Graph}, except the graph data is hidden,
  like in a class interface.
  Refer to the documentation of {!Storage} for information about
  the functions in the fields.*)
type graph = {
  name : unit -> Rdf_uri.uri;
  size : unit -> int ;
  add_triple :
    sub:Rdf_term.term -> pred:Rdf_uri.uri -> obj:Rdf_term.term -> unit;
  rem_triple :
    sub:Rdf_term.term -> pred:Rdf_uri.uri -> obj:Rdf_term.term -> unit;
  add_triple_t : Rdf_term.triple -> unit;
  rem_triple_t : Rdf_term.triple -> unit;
  subjects_of : pred:Rdf_uri.uri -> obj:Rdf_term.term -> Rdf_term.term list;
  predicates_of :
    sub:Rdf_term.term -> obj:Rdf_term.term -> Rdf_uri.uri list;
  objects_of : sub:Rdf_term.term -> pred:Rdf_uri.uri -> Rdf_term.term list;
  find :
    ?sub:Rdf_term.term ->
    ?pred:Rdf_uri.uri -> ?obj:Rdf_term.term -> unit -> Rdf_term.triple list;
  exists :
    ?sub:Rdf_term.term ->
    ?pred:Rdf_uri.uri -> ?obj:Rdf_term.term -> unit -> bool;
  exists_t : Rdf_term.triple -> bool;
  subjects : unit -> Rdf_term.term list;
  predicates : unit -> Rdf_uri.uri list;
  objects : unit -> Rdf_term.term list;
  transaction_start : unit -> unit;
  transaction_commit : unit -> unit;
  transaction_rollback : unit -> unit;
  new_blank_id : unit -> Rdf_term.blank_id ;
  namespaces : unit -> (Rdf_uri.uri * string) list ;
  bgp : (module Rdf_bgp.S) ;
}

(** {2 Graph creation} *)

(** [open_graph ~options uri_name] creates a new graph. The storage used
  is specified by the "storage" option. For example, having [("storage", "mysql")]
  in the options indicates to use the storage "mysql".

  If the specified storage is not registered, the function raises [Failure].
  Other options may be used by each storage.

  To make sure the storage you want to use is registered, beware of linking the
  corresponding module in your executable, either by using the [-linkall] option
  or by adding a reference to the module in your code.
*)
val open_graph : ?options:(string * string) list -> Rdf_uri.uri -> graph

(** [merge g1 g2] add triples from [g2] to [g1].*)
val merge : graph -> graph -> unit

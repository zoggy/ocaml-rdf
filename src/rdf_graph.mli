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
let g = Rdf_graph.open_graph ~options (Rdf_uri.uri "http://hello.fr") in
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

(** {2 The storage interface} *)

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

    (** Adding a triple to the graph. *)
    val add_triple :
      g ->
      sub:Rdf_node.node -> pred:Rdf_node.node -> obj:Rdf_node.node -> unit

    (** Removing a triple from the graph. *)
    val rem_triple :
      g ->
      sub:Rdf_node.node -> pred:Rdf_node.node -> obj:Rdf_node.node -> unit

    (** Adding a triple to the graph, curryfied form. *)
    val add_triple_t : g -> Rdf_node.triple -> unit

    (** Removing a triple from the graph, curryfied form. *)
    val rem_triple_t : g -> Rdf_node.triple -> unit

    (** {3 Querying the graph} *)

    (** [subjects_of g ~pred ~obj] returns the list of nodes which are
      subjects in triples with the specified predicate and object. *)
    val subjects_of :
      g -> pred:Rdf_node.node -> obj:Rdf_node.node -> Rdf_node.node list

    (** [predicates_of g ~sub ~obj] returns the list of nodes which are
      predicates in triples with the specified subject and object. *)
    val predicates_of :
      g -> sub:Rdf_node.node -> obj:Rdf_node.node -> Rdf_node.node list

    (** [objects_of g ~sub ~pred] returns the list of nodes which are
      objects in triples with the specified subject and predicate. *)
    val objects_of :
      g -> sub:Rdf_node.node -> pred:Rdf_node.node -> Rdf_node.node list

    (** [find ?sub ?pred ?obj g] returns the list of triples matching the
         constraints given by the optional subject, predicate and object.
         One can specify, zero, one, two or three of these nodes. *)
    val find :
      ?sub:Rdf_node.node ->
      ?pred:Rdf_node.node -> ?obj:Rdf_node.node -> g -> Rdf_node.triple list

    (** Same as {!find} but only returns [true] if at least one triple
      of the graph matches the constraints. *)
    val exists :
      ?sub:Rdf_node.node ->
      ?pred:Rdf_node.node -> ?obj:Rdf_node.node -> g -> bool

    (** Curryfied version of {!exists}. *)
    val exists_t : Rdf_node.triple -> g -> bool

    (** Return the list of nodes appearing in subject position. *)
    val subjects : g -> Rdf_node.node list

    (** Return the list of nodes appearing in predicate position. *)
    val predicates : g -> Rdf_node.node list

    (** Return the list of nodes appearing in object position. *)
    val objects : g -> Rdf_node.node list

    (** {3 Transactions} *)

    (** Start a transaction. All storage may not support transactions.*)
    val transaction_start : g -> unit

    (** Commit. *)
    val transaction_commit : g -> unit

    (** Rollback. *)
    val transaction_rollback : g -> unit
  end

(** Storage name * message * original exception *)
exception Storage_error of string * string * exn

module type Graph =
  sig
    type g
    val open_graph : ?options:(string * string) list -> Rdf_uri.uri -> g
    val graph_name : g -> Rdf_uri.uri
    val add_triple :
      g ->
      sub:Rdf_node.node -> pred:Rdf_node.node -> obj:Rdf_node.node -> unit
    val rem_triple :
      g ->
      sub:Rdf_node.node -> pred:Rdf_node.node -> obj:Rdf_node.node -> unit
    val add_triple_t : g -> Rdf_node.triple -> unit
    val rem_triple_t : g -> Rdf_node.triple -> unit
    val subjects_of :
      g -> pred:Rdf_node.node -> obj:Rdf_node.node -> Rdf_node.node list
    val predicates_of :
      g -> sub:Rdf_node.node -> obj:Rdf_node.node -> Rdf_node.node list
    val objects_of :
      g -> sub:Rdf_node.node -> pred:Rdf_node.node -> Rdf_node.node list
    val find :
      ?sub:Rdf_node.node ->
      ?pred:Rdf_node.node -> ?obj:Rdf_node.node -> g -> Rdf_node.triple list
    val exists :
      ?sub:Rdf_node.node ->
      ?pred:Rdf_node.node -> ?obj:Rdf_node.node -> g -> bool
    val exists_t : Rdf_node.triple -> g -> bool
    val subjects : g -> Rdf_node.node list
    val predicates : g -> Rdf_node.node list
    val objects : g -> Rdf_node.node list
    val transaction_start : g -> unit
    val transaction_commit : g -> unit
    val transaction_rollback : g -> unit
  end
module Make : functor (S : Storage) -> Graph with type g = S.g

val storages : (string * (module Graph)) list ref
val add_storage : (module Storage) -> unit
type graph = {
  name : unit -> Rdf_uri.uri;
  add_triple :
    sub:Rdf_node.node -> pred:Rdf_node.node -> obj:Rdf_node.node -> unit;
  rem_triple :
    sub:Rdf_node.node -> pred:Rdf_node.node -> obj:Rdf_node.node -> unit;
  add_triple_t : Rdf_node.node * Rdf_node.node * Rdf_node.node -> unit;
  rem_triple_t : Rdf_node.node * Rdf_node.node * Rdf_node.node -> unit;
  subjects_of : pred:Rdf_node.node -> obj:Rdf_node.node -> Rdf_node.node list;
  predicates_of :
    sub:Rdf_node.node -> obj:Rdf_node.node -> Rdf_node.node list;
  objects_of : sub:Rdf_node.node -> pred:Rdf_node.node -> Rdf_node.node list;
  find :
    ?sub:Rdf_node.node ->
    ?pred:Rdf_node.node -> ?obj:Rdf_node.node -> unit -> Rdf_node.triple list;
  exists :
    ?sub:Rdf_node.node ->
    ?pred:Rdf_node.node -> ?obj:Rdf_node.node -> unit -> bool;
  exists_t : Rdf_node.triple -> bool;
  subjects : unit -> Rdf_node.node list;
  predicates : unit -> Rdf_node.node list;
  objects : unit -> Rdf_node.node list;
  transaction_start : unit -> unit;
  transaction_commit : unit -> unit;
  transaction_rollback : unit -> unit;
}
val open_graph : ?options:(string * string) list -> Rdf_uri.uri -> graph

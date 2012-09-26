(** Nodes. *)

(** Literal nodes contain a value, a optional language and an option data type URI. *)
type literal = {
  lit_value : string;
  lit_language : string option;
  lit_type : Rdf_uri.uri option;
}

(** Type for blank node ids. *)
type blank_id

(** Various kinds of nodes. *)
type node =
  | Uri of Rdf_uri.uri
  | Literal of literal
  | Blank
  | Blank_ of blank_id

module Ord_type : sig type t = node val compare : node -> node -> int end

(** A RDF triple is just ... a triple of nodes. *)
type triple = node * node * node

(** Get a string from a blank node id. *)
val string_of_blank_id : blank_id -> string

(** Make a blank node id from a string. *)
val blank_id_of_string : string -> blank_id

(** Shortcut for [Uri (Rdf_uri.uri string)]. *)
val node_of_uri_string : string -> node

(** Creation of a literal. *)
val mk_literal : ?typ:Rdf_uri.uri -> ?lang:string -> string -> literal


(** Create a datetime literal with type uri from the given datetime [d].
  If no date is given, [Unix.time()] is used.*)
val mk_literal_datetime : ?d:float -> unit -> literal

(** Create a literal node from the given datetime. (see {!mk_literal_datetime}). *)
val node_of_datetime : ?d:float -> unit -> node

(** Parse a literal to get a datetime. *)
val datetime_of_literal : literal -> Netdate.t


(** Create a boolean literal with type uri from the given boolean. *)
val mk_literal_bool : bool -> literal

(** Create a literal node from the given boolean. (see {!mk_literal_bool}). *)
val node_of_bool : bool -> node

(** Parse a literal to get a boolean. *)
val bool_of_literal : literal -> bool


(** Shortcut for [Literal (mk_literal ?typ ?lang string)] *)
val node_of_literal_string : ?typ:Rdf_uri.uri -> ?lang:string -> string -> node


(** Create a string for the given node, using RDF turtle syntax conventions.
  @see <http://www.w3.org/TeamSubmission/turtle/#language> the description of turtle language. *)
val string_of_node : node -> string

(** [node_hash node] returns an int64 identifiying (hopefully unically)) a given node. *)
val node_hash : node -> int64

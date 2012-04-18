(** Base types. *)

type literal = {
  lit_value : string;
  lit_language : string option;
  lit_type : Rdf_uri.uri option;
}

type blank_id

type node =
  | Uri of Rdf_uri.uri
  | Literal of literal
  | Blank
  | Blank_ of blank_id

type triple = node * node * node

val string_of_blank_id : blank_id -> string
val blank_id_of_string : string -> blank_id

val node_of_uri_string : string -> node

val mk_literal : ?typ:Rdf_uri.uri -> ?lang:string -> string -> literal
val node_of_literal_string : ?typ:Rdf_uri.uri -> ?lang:string -> string -> node

val string_of_node : node -> string

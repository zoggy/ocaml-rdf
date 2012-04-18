(** Base types. *)

type uri
type literal = {
  lit_value : string;
  lit_language : string option;
  lit_type : uri option;
}

type blank_id

type node =
  | Uri of uri
  | Literal of literal
  | Blank
  | Blank_ of blank_id

type triple = node * node * node

val string_of_uri : uri -> string
val uri_of_string : string -> uri

val string_of_blank_id : blank_id -> string
val blank_id_of_string : string -> blank_id

val node_of_uri_string : string -> node

val mk_literal : ?typ:uri -> ?lang:string -> string -> literal
val node_of_literal_string : ?typ:uri -> ?lang:string -> string -> node

val string_of_node : node -> string

type options = (string * string) list
val get_option : ?def:string-> string -> options -> string

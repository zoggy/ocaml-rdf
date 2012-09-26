(** Reading and writing RDF/XML. *)

exception Invalid_rdf of string

val from_string : Rdf_graph.graph -> base: Rdf_uri.uri -> string -> unit
val from_file : Rdf_graph.graph -> base: Rdf_uri.uri -> string -> unit

val to_string :
  ?namespaces: (Rdf_uri.uri * string) list -> Rdf_graph.graph -> string

val to_file :
  ?namespaces: (Rdf_uri.uri * string) list ->
    Rdf_graph.graph -> string -> unit
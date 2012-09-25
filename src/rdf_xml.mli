(** Reading and writing RDF/XML. *)

exception Invalid_rdf of string

val input_string : Rdf_graph.graph -> base: Rdf_uri.uri -> string -> unit
val input_file : Rdf_graph.graph -> base: Rdf_uri.uri -> string -> unit

val output_file : Rdf_graph.graph ->
  ?namespaces: (string * Rdf_uri.uri) list -> string -> unit
(** Reading and writing RDF/XML. *)

exception Invalid_rdf of string

val input_file : Rdf_graph.graph -> base: Rdf_uri.uri -> string -> unit
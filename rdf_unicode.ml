(** Unicode - Unicode utility functions.
  @rdfmod redland-unicode.html
  @rdfprefix librdf_
*)

(** @rdf utf8_to_latin1 *)
external utf8_to_latin1 : string -> string = "ml_librdf_utf8_to_latin1"

(** @rdf latin1_to_utf8 *)
external latin1_to_utf8 : string -> string = "ml_librdf_latin1_to_utf8"

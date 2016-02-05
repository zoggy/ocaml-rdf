(** Elements of [http://www.w3.org/1999/02/22-rdf-syntax-ns#] *)

(** [http://www.w3.org/1999/02/22-rdf-syntax-ns#] *)
val rdf : Iri.t
val rdf_ : string -> Iri.t

(** The class of containers of alternatives. *)
val alt : Iri.t

(** The class of unordered containers. *)
val bag : Iri.t

(** The first item in the subject RDF list. *)
val first : Iri.t

(** The datatype of RDF literals storing fragments of HTML content *)
val hTML : Iri.t

(** The datatype of language-tagged string values *)
val langString : Iri.t

(** The class of RDF Lists. *)
val list : Iri.t

(** The object of the subject RDF statement. *)
val object_ : Iri.t

(** The class of plain (i.e. untyped) literal values, as used in RIF and OWL 2 *)
val plainLiteral : Iri.t

(** The predicate of the subject RDF statement. *)
val predicate : Iri.t

(** The class of RDF properties. *)
val property : Iri.t

(** The rest of the subject RDF list after the first item. *)
val rest : Iri.t

(** The class of ordered containers. *)
val seq : Iri.t

(** The class of RDF statements. *)
val statement : Iri.t

(** The subject of the subject RDF statement. *)
val subject : Iri.t

(** The subject is an instance of a class. *)
val type_ : Iri.t

(** Idiomatic property used for structured values. *)
val value : Iri.t

(** The datatype of XML literal values. *)
val xMLLiteral : Iri.t


module Open : sig
  (** The class of containers of alternatives. *)
  val rdf_alt : Iri.t

  (** The class of unordered containers. *)
  val rdf_bag : Iri.t

  (** The first item in the subject RDF list. *)
  val rdf_first : Iri.t

  (** The datatype of RDF literals storing fragments of HTML content *)
  val rdf_hTML : Iri.t

  (** The datatype of language-tagged string values *)
  val rdf_langString : Iri.t

  (** The class of RDF Lists. *)
  val rdf_list : Iri.t

  (** The object of the subject RDF statement. *)
  val rdf_object : Iri.t

  (** The class of plain (i.e. untyped) literal values, as used in RIF and OWL 2 *)
  val rdf_plainLiteral : Iri.t

  (** The predicate of the subject RDF statement. *)
  val rdf_predicate : Iri.t

  (** The class of RDF properties. *)
  val rdf_property : Iri.t

  (** The rest of the subject RDF list after the first item. *)
  val rdf_rest : Iri.t

  (** The class of ordered containers. *)
  val rdf_seq : Iri.t

  (** The class of RDF statements. *)
  val rdf_statement : Iri.t

  (** The subject of the subject RDF statement. *)
  val rdf_subject : Iri.t

  (** The subject is an instance of a class. *)
  val rdf_type : Iri.t

  (** Idiomatic property used for structured values. *)
  val rdf_value : Iri.t

  (** The datatype of XML literal values. *)
  val rdf_xMLLiteral : Iri.t

end

(** Elements of [http://www.w3.org/1999/02/22-rdf-syntax-ns#] *)

(** [http://www.w3.org/1999/02/22-rdf-syntax-ns#] *)
val rdf : Iri.t
val rdf_ : string -> Iri.t

(** [http://www.w3.org/2001/XMLSchema#] *)
val xsd : Iri.t
val xsd_ : string -> Iri.t

val about : Iri.t
val datatype : Iri.t
val description : Iri.t
val id : Iri.t
val li : Iri.t
val nodeID : Iri.t
val _RDF : Iri.t
val parseType : Iri.t
val resource : Iri.t

(** The class of containers of alternatives. *)
val c_Alt : Iri.t

(** The class of unordered containers. *)
val c_Bag : Iri.t

(** The first item in the subject RDF list. *)
val first : Iri.t

(** The datatype of RDF literals storing fragments of HTML content *)
val dt_HTML : Iri.t

(** The datatype of language-tagged string values *)
val dt_langString : Iri.t

(** The class of RDF Lists. *)
val c_List : Iri.t

(** The object of the subject RDF statement. *)
val object_ : Iri.t

(** The class of plain (i.e. untyped) literal values, as used in RIF and OWL 2 *)
val dt_PlainLiteral : Iri.t

(** The predicate of the subject RDF statement. *)
val predicate : Iri.t

(** The class of RDF properties. *)
val c_Property : Iri.t

(** The rest of the subject RDF list after the first item. *)
val rest : Iri.t

(** The class of ordered containers. *)
val c_Seq : Iri.t

(** The class of RDF statements. *)
val c_Statement : Iri.t

(** The subject of the subject RDF statement. *)
val subject : Iri.t

(** The subject is an instance of a class. *)
val type_ : Iri.t

(** Idiomatic property used for structured values. *)
val value : Iri.t

(** The datatype of XML literal values. *)
val dt_XMLLiteral : Iri.t

val n : int -> Iri.t
val nil : Iri.t

(** {3 XML Schema datatypes} *)

val xsd_integer : Iri.t
val xsd_double : Iri.t
val xsd_decimal : Iri.t
val xsd_boolean : Iri.t
val xsd_string : Iri.t
val xsd_datetime : Iri.t

module Open :
  sig
    val rdf_about : Iri.t
    val rdf_datatype : Iri.t
    val rdf_description : Iri.t
    val rdf_id : Iri.t
    val rdf_li : Iri.t
    val rdf_nodeID : Iri.t
    val rdf_parseType : Iri.t
    val rdf_RDF : Iri.t
    val rdf_resource : Iri.t

  (** The class of containers of alternatives. *)
  val rdf_c_Alt : Iri.t

  (** The class of unordered containers. *)
  val rdf_c_Bag : Iri.t

  (** The first item in the subject RDF list. *)
  val rdf_first : Iri.t

  (** The datatype of RDF literals storing fragments of HTML content *)
  val rdf_dt_HTML : Iri.t

  (** The datatype of language-tagged string values *)
  val rdf_dt_langString : Iri.t

  (** The class of RDF Lists. *)
  val rdf_c_List : Iri.t

  (** The object of the subject RDF statement. *)
  val rdf_object : Iri.t

  (** The class of plain (i.e. untyped) literal values, as used in RIF and OWL 2 *)
  val rdf_dt_PlainLiteral : Iri.t

  (** The predicate of the subject RDF statement. *)
  val rdf_predicate : Iri.t

  (** The class of RDF properties. *)
  val rdf_c_Property : Iri.t

  (** The rest of the subject RDF list after the first item. *)
  val rdf_rest : Iri.t

  (** The class of ordered containers. *)
  val rdf_c_Seq : Iri.t

  (** The class of RDF statements. *)
  val rdf_c_Statement : Iri.t

  (** The subject of the subject RDF statement. *)
  val rdf_subject : Iri.t

  (** The subject is an instance of a class. *)
  val rdf_type : Iri.t

  (** Idiomatic property used for structured values. *)
  val rdf_value : Iri.t

  (** The datatype of XML literal values. *)
  val rdf_dt_XMLLiteral : Iri.t

  val rdf_n : int -> Iri.t
  val rdf_nil : Iri.t
  val xsd_integer : Iri.t
  val xsd_double : Iri.t
  val xsd_decimal : Iri.t
  val xsd_boolean : Iri.t
  val xsd_string : Iri.t
  val xsd_datetime : Iri.t
  end

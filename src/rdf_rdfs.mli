(** Elements of [http://www.w3.org/2000/01/rdf-schema#] *)

(** [http://www.w3.org/2000/01/rdf-schema#] *)
val rdfs : Iri.t
val rdfs_ : string -> Iri.t

(** The class of classes. *)
val class_ : Iri.t

(** A description of the subject resource. *)
val comment : Iri.t

(** The class of RDF containers. *)
val container : Iri.t

(** The class of container membership properties, rdf:_1, rdf:_2, ...,
                    all of which are sub-properties of 'member'. *)
val containerMembershipProperty : Iri.t

(** The class of RDF datatypes. *)
val datatype : Iri.t

(** A domain of the subject property. *)
val domain : Iri.t

(** The defininition of the subject resource. *)
val isDefinedBy : Iri.t

(** A human-readable name for the subject. *)
val label : Iri.t

(** The class of literal values, eg. textual strings and integers. *)
val literal : Iri.t

(** A member of the subject resource. *)
val member : Iri.t

(** A range of the subject property. *)
val range : Iri.t

(** The class resource, everything. *)
val resource : Iri.t

(** Further information about the subject resource. *)
val seeAlso : Iri.t

(** The subject is a subclass of a class. *)
val subClassOf : Iri.t

(** The subject is a subproperty of a property. *)
val subPropertyOf : Iri.t


module Open : sig
  (** The class of classes. *)
  val rdfs_class : Iri.t

  (** A description of the subject resource. *)
  val rdfs_comment : Iri.t

  (** The class of RDF containers. *)
  val rdfs_container : Iri.t

  (** The class of container membership properties, rdf:_1, rdf:_2, ...,
                    all of which are sub-properties of 'member'. *)
  val rdfs_containerMembershipProperty : Iri.t

  (** The class of RDF datatypes. *)
  val rdfs_datatype : Iri.t

  (** A domain of the subject property. *)
  val rdfs_domain : Iri.t

  (** The defininition of the subject resource. *)
  val rdfs_isDefinedBy : Iri.t

  (** A human-readable name for the subject. *)
  val rdfs_label : Iri.t

  (** The class of literal values, eg. textual strings and integers. *)
  val rdfs_literal : Iri.t

  (** A member of the subject resource. *)
  val rdfs_member : Iri.t

  (** A range of the subject property. *)
  val rdfs_range : Iri.t

  (** The class resource, everything. *)
  val rdfs_resource : Iri.t

  (** Further information about the subject resource. *)
  val rdfs_seeAlso : Iri.t

  (** The subject is a subclass of a class. *)
  val rdfs_subClassOf : Iri.t

  (** The subject is a subproperty of a property. *)
  val rdfs_subPropertyOf : Iri.t

end

(** {2 Building vocabulary descriptions} *)

(** Add usual [rdf] and [rdfs] namespaces in the given graph. *)
val add_namespaces : Rdf_graph.graph -> unit

val property : Rdf_graph.graph ->
  label: string ->
    ?label_lang: (string * string) list ->
    ?comment: string ->
    ?comment_lang: (string * string) list ->
    ?domains: Iri.t list ->
    ?ranges: Iri.t list ->
    ?subof: Iri.t ->
    ?more: (Iri.t * Rdf_term.term) list ->
    Iri.t -> unit

val class_ : Rdf_graph.graph ->
  label: string ->
    ?label_lang: (string * string) list ->
    ?comment: string ->
    ?comment_lang: (string * string) list ->
    ?subof: Iri.t ->
    ?more: (Iri.t * Rdf_term.term) list ->
    Iri.t -> unit

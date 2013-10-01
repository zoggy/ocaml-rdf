(** Elements of [http://www.w3.org/2000/01/rdf-schema#] *)

(** [http://www.w3.org/2000/01/rdf-schema#] *)
val rdfs : Rdf_iri.iri
val rdfs_ : string -> Rdf_iri.iri

(** The class of classes. *)
val rdfs_Class : Rdf_iri.iri

(** A description of the subject resource. *)
val rdfs_comment : Rdf_iri.iri

(** The class of RDF containers. *)
val rdfs_Container : Rdf_iri.iri

(** The class of container membership properties, rdf:_1, rdf:_2, ..., all of which are sub-properties of 'member'. *)
val rdfs_ContainerMembershipProperty : Rdf_iri.iri

(** The class of RDF datatypes. *)
val rdfs_Datatype : Rdf_iri.iri

(** A domain of the subject property. *)
val rdfs_domain : Rdf_iri.iri

(** The defininition of the subject resource. *)
val rdfs_isDefinedBy : Rdf_iri.iri

(** A human-readable name for the subject. *)
val rdfs_label : Rdf_iri.iri

(** The class of literal values, eg. textual strings and integers. *)
val rdfs_Literal : Rdf_iri.iri

(** A member of the subject resource. *)
val rdfs_member : Rdf_iri.iri

(** A range of the subject property. *)
val rdfs_range : Rdf_iri.iri

(** The class resource, everything. *)
val rdfs_Resource : Rdf_iri.iri

(** Further information about the subject resource. *)
val rdfs_seeAlso : Rdf_iri.iri

(** The subject is a subclass of a class. *)
val rdfs_subClassOf : Rdf_iri.iri

(** The subject is a subproperty of a property. *)
val rdfs_subPropertyOf : Rdf_iri.iri

(** {2 Building vocabulary descriptions} *)

(** Add usual [rdf] and [rdfs] namespaces in the given graph. *)
val add_namespaces : Rdf_graph.graph -> unit

val property : Rdf_graph.graph ->
  label: string ->
    ?label_lang: (string * string) list ->
    ?comment: string ->
    ?comment_lang: (string * string) list ->
    ?domains: Rdf_iri.iri list ->
    ?ranges: Rdf_iri.iri list ->
    ?subof: Rdf_iri.iri ->
    ?more: (Rdf_iri.iri * Rdf_term.term) list ->
    Rdf_iri.iri -> unit

val class_ : Rdf_graph.graph ->
  label: string ->
    ?label_lang: (string * string) list ->
    ?comment: string ->
    ?comment_lang: (string * string) list ->
    ?subof: Rdf_iri.iri ->
    ?more: (Rdf_iri.iri * Rdf_term.term) list ->
    Rdf_iri.iri -> unit


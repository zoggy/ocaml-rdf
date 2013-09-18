(** Elements of [http://www.w3.org/2000/01/rdf-schema#] *)

(** [http://www.w3.org/2000/01/rdf-schema#] *)
val rdfs : Rdf_uri.uri
val rdfs_ : string -> Rdf_uri.uri

(** The class of classes. *)
val rdfs_Class : Rdf_uri.uri

(** A description of the subject resource. *)
val rdfs_comment : Rdf_uri.uri

(** The class of RDF containers. *)
val rdfs_Container : Rdf_uri.uri

(** The class of container membership properties, rdf:_1, rdf:_2, ..., all of which are sub-properties of 'member'. *)
val rdfs_ContainerMembershipProperty : Rdf_uri.uri

(** The class of RDF datatypes. *)
val rdfs_Datatype : Rdf_uri.uri

(** A domain of the subject property. *)
val rdfs_domain : Rdf_uri.uri

(** The defininition of the subject resource. *)
val rdfs_isDefinedBy : Rdf_uri.uri

(** A human-readable name for the subject. *)
val rdfs_label : Rdf_uri.uri

(** The class of literal values, eg. textual strings and integers. *)
val rdfs_Literal : Rdf_uri.uri

(** A member of the subject resource. *)
val rdfs_member : Rdf_uri.uri

(** A range of the subject property. *)
val rdfs_range : Rdf_uri.uri

(** The class resource, everything. *)
val rdfs_Resource : Rdf_uri.uri

(** Further information about the subject resource. *)
val rdfs_seeAlso : Rdf_uri.uri

(** The subject is a subclass of a class. *)
val rdfs_subClassOf : Rdf_uri.uri

(** The subject is a subproperty of a property. *)
val rdfs_subPropertyOf : Rdf_uri.uri


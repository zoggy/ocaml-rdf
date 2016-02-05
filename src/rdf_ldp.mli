(** Elements of [http://www.w3.org/ns/ldp#] *)

(** [http://www.w3.org/ns/ldp#] *)
val ldp : Iri.t
val ldp_ : string -> Iri.t

(** An LDPC that uses a predefined predicate to simply link to its contained resources. *)
val basicContainer : Iri.t

(** Links a resource with constraints that the server requires requests like creation and update to conform to. *)
val constrainedBy : Iri.t

(** A Linked Data Platform RDF Source (LDP-RS) that also conforms to additional patterns and conventions for managing membership. Readers should refer to the specification defining this ontology for the list of behaviors associated with it. *)
val container : Iri.t

(** Links a container with resources created through the container. *)
val contains : Iri.t

(** An LDPC that is similar to a LDP-DC but it allows an indirection with the ability to list as member a resource, such as a URI representing a real-world object, that is different from the resource that is created. *)
val directContainer : Iri.t

(** Indicates which predicate is used in membership triples, and that the membership triple pattern is < membership-constant-URI , object-of-hasMemberRelation, member-URI >. *)
val hasMemberRelation : Iri.t

(** An LDPC that has the flexibility of choosing what form the membership triples take. *)
val indirectContainer : Iri.t

(** Indicates which triple in a creation request should be used as the member-URI value in the membership triple added when the creation request is successful. *)
val insertedContentRelation : Iri.t

(** Indicates which predicate is used in membership triples, and that the membership triple pattern is < member-URI , object-of-isMemberOfRelation, membership-constant-URI >. *)
val isMemberOfRelation : Iri.t

(** LDP servers should use this predicate as the membership predicate if there is no obvious predicate from an application vocabulary to use. *)
val member : Iri.t

(** Indicates the membership-constant-URI in a membership triple.  Depending upon the membership triple pattern a container uses, as indicated by the presence of ldp:hasMemberRelation or ldp:isMemberOfRelation, the membership-constant-URI might occupy either the subject or object position in membership triples. *)
val membershipResource : Iri.t

(** A Linked Data Platform Resource (LDPR) whose state is NOT represented as RDF. *)
val nonRDFSource : Iri.t

(** URI signifying that the resource is an in-sequence page resource, as defined by LDP Paging.  Typically used on Link rel='type' response headers. *)
val page : Iri.t

(** Link to a page sequence resource, as defined by LDP Paging.  Typically used to communicate the sorting criteria used to allocate LDPC members to pages. *)
val pageSequence : Iri.t

(** The collation used to order the members across pages in a page sequence when comparing strings. *)
val pageSortCollation : Iri.t

(** Link to the list of sorting criteria used by the server in a representation.  Typically used on Link response headers as an extension link relation URI in the rel= parameter. *)
val pageSortCriteria : Iri.t

(** Element in the list of sorting criteria used by the server to assign container members to pages. *)
val pageSortCriterion : Iri.t

(** The ascending/descending/etc order used to order the members across pages in a page sequence. *)
val pageSortOrder : Iri.t

(** Predicate used to specify the order of the members across a page sequence's in-sequence page resources; it asserts nothing about the order of members in the representation of a single page. *)
val pageSortPredicate : Iri.t

(** A Linked Data Platform Resource (LDPR) whose state is represented as RDF. *)
val rDFSource : Iri.t

(** A HTTP-addressable resource whose lifecycle is managed by a LDP server. *)
val resource : Iri.t


module Open : sig
  (** An LDPC that uses a predefined predicate to simply link to its contained resources. *)
  val ldp_basicContainer : Iri.t

  (** Links a resource with constraints that the server requires requests like creation and update to conform to. *)
  val ldp_constrainedBy : Iri.t

  (** A Linked Data Platform RDF Source (LDP-RS) that also conforms to additional patterns and conventions for managing membership. Readers should refer to the specification defining this ontology for the list of behaviors associated with it. *)
  val ldp_container : Iri.t

  (** Links a container with resources created through the container. *)
  val ldp_contains : Iri.t

  (** An LDPC that is similar to a LDP-DC but it allows an indirection with the ability to list as member a resource, such as a URI representing a real-world object, that is different from the resource that is created. *)
  val ldp_directContainer : Iri.t

  (** Indicates which predicate is used in membership triples, and that the membership triple pattern is < membership-constant-URI , object-of-hasMemberRelation, member-URI >. *)
  val ldp_hasMemberRelation : Iri.t

  (** An LDPC that has the flexibility of choosing what form the membership triples take. *)
  val ldp_indirectContainer : Iri.t

  (** Indicates which triple in a creation request should be used as the member-URI value in the membership triple added when the creation request is successful. *)
  val ldp_insertedContentRelation : Iri.t

  (** Indicates which predicate is used in membership triples, and that the membership triple pattern is < member-URI , object-of-isMemberOfRelation, membership-constant-URI >. *)
  val ldp_isMemberOfRelation : Iri.t

  (** LDP servers should use this predicate as the membership predicate if there is no obvious predicate from an application vocabulary to use. *)
  val ldp_member : Iri.t

  (** Indicates the membership-constant-URI in a membership triple.  Depending upon the membership triple pattern a container uses, as indicated by the presence of ldp:hasMemberRelation or ldp:isMemberOfRelation, the membership-constant-URI might occupy either the subject or object position in membership triples. *)
  val ldp_membershipResource : Iri.t

  (** A Linked Data Platform Resource (LDPR) whose state is NOT represented as RDF. *)
  val ldp_nonRDFSource : Iri.t

  (** URI signifying that the resource is an in-sequence page resource, as defined by LDP Paging.  Typically used on Link rel='type' response headers. *)
  val ldp_page : Iri.t

  (** Link to a page sequence resource, as defined by LDP Paging.  Typically used to communicate the sorting criteria used to allocate LDPC members to pages. *)
  val ldp_pageSequence : Iri.t

  (** The collation used to order the members across pages in a page sequence when comparing strings. *)
  val ldp_pageSortCollation : Iri.t

  (** Link to the list of sorting criteria used by the server in a representation.  Typically used on Link response headers as an extension link relation URI in the rel= parameter. *)
  val ldp_pageSortCriteria : Iri.t

  (** Element in the list of sorting criteria used by the server to assign container members to pages. *)
  val ldp_pageSortCriterion : Iri.t

  (** The ascending/descending/etc order used to order the members across pages in a page sequence. *)
  val ldp_pageSortOrder : Iri.t

  (** Predicate used to specify the order of the members across a page sequence's in-sequence page resources; it asserts nothing about the order of members in the representation of a single page. *)
  val ldp_pageSortPredicate : Iri.t

  (** A Linked Data Platform Resource (LDPR) whose state is represented as RDF. *)
  val ldp_rDFSource : Iri.t

  (** A HTTP-addressable resource whose lifecycle is managed by a LDP server. *)
  val ldp_resource : Iri.t

end

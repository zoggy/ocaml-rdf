(** Elements of [http://www.w3.org/2002/07/owl#] *)

(** [http://www.w3.org/2002/07/owl#] *)
val owl : Iri.t
val owl_ : string -> Iri.t

(** The class of collections of pairwise different individuals. *)
val c_AllDifferent : Iri.t

(** The class of collections of pairwise disjoint classes. *)
val c_AllDisjointClasses : Iri.t

(** The class of collections of pairwise disjoint properties. *)
val c_AllDisjointProperties : Iri.t

(** The property that determines the class that a universal property restriction refers to. *)
val allValuesFrom : Iri.t

(** The property that determines the predicate of an annotated axiom or annotated annotation. *)
val annotatedProperty : Iri.t

(** The property that determines the subject of an annotated axiom or annotated annotation. *)
val annotatedSource : Iri.t

(** The property that determines the object of an annotated axiom or annotated annotation. *)
val annotatedTarget : Iri.t

(** The class of annotated annotations for which the RDF serialization consists of an annotated subject, predicate and object. *)
val c_Annotation : Iri.t

(** The class of annotation properties. *)
val c_AnnotationProperty : Iri.t

(** The property that determines the predicate of a negative property assertion. *)
val assertionProperty : Iri.t

(** The class of asymmetric properties. *)
val c_AsymmetricProperty : Iri.t

(** The class of annotated axioms for which the RDF serialization consists of an annotated subject, predicate and object. *)
val c_Axiom : Iri.t

(** The property that determines the cardinality of an exact cardinality restriction. *)
val cardinality : Iri.t

(** The class of OWL classes. *)
val c_Class : Iri.t

(** The property that determines that a given class is the complement of another class. *)
val complementOf : Iri.t

(** The class of OWL data ranges, which are special kinds of datatypes. Note: The use of the IRI owl:DataRange has been deprecated as of OWL 2. The IRI rdfs:Datatype SHOULD be used instead. *)
val c_DataRange : Iri.t

(** The property that determines that a given data range is the complement of another data range with respect to the data domain. *)
val datatypeComplementOf : Iri.t

(** The class of data properties. *)
val c_DatatypeProperty : Iri.t

(** The class of deprecated classes. *)
val c_DeprecatedClass : Iri.t

(** The class of deprecated properties. *)
val c_DeprecatedProperty : Iri.t

(** The property that determines that two given individuals are different. *)
val differentFrom : Iri.t

(** The property that determines that a given class is equivalent to the disjoint union of a collection of other classes. *)
val disjointUnionOf : Iri.t

(** The property that determines that two given classes are disjoint. *)
val disjointWith : Iri.t

(** The property that determines the collection of pairwise different individuals in a owl:AllDifferent axiom. *)
val distinctMembers : Iri.t

(** The property that determines that two given classes are equivalent, and that is used to specify datatype definitions. *)
val equivalentClass : Iri.t

(** The property that determines that two given properties are equivalent. *)
val equivalentProperty : Iri.t

(** The class of functional properties. *)
val c_FunctionalProperty : Iri.t

(** The property that determines the collection of properties that jointly build a key. *)
val hasKey : Iri.t

(** The property that determines the property that a self restriction refers to. *)
val hasSelf : Iri.t

(** The property that determines the individual that a has-value restriction refers to. *)
val hasValue : Iri.t

(** The property that determines the collection of classes or data ranges that build an intersection. *)
val intersectionOf : Iri.t

(** The class of inverse-functional properties. *)
val c_InverseFunctionalProperty : Iri.t

(** The property that determines that two given properties are inverse. *)
val inverseOf : Iri.t

(** The class of irreflexive properties. *)
val c_IrreflexiveProperty : Iri.t

(** The property that determines the cardinality of a maximum cardinality restriction. *)
val maxCardinality : Iri.t

(** The property that determines the cardinality of a maximum qualified cardinality restriction. *)
val maxQualifiedCardinality : Iri.t

(** The property that determines the collection of members in either a owl:AllDifferent, owl:AllDisjointClasses or owl:AllDisjointProperties axiom. *)
val members : Iri.t

(** The property that determines the cardinality of a minimum cardinality restriction. *)
val minCardinality : Iri.t

(** The property that determines the cardinality of a minimum qualified cardinality restriction. *)
val minQualifiedCardinality : Iri.t

(** The class of named individuals. *)
val c_NamedIndividual : Iri.t

(** The class of negative property assertions. *)
val c_NegativePropertyAssertion : Iri.t

(** The class of object properties. *)
val c_ObjectProperty : Iri.t

(** The property that determines the class that a qualified object cardinality restriction refers to. *)
val onClass : Iri.t

(** The property that determines the data range that a qualified data cardinality restriction refers to. *)
val onDataRange : Iri.t

(** The property that determines the datatype that a datatype restriction refers to. *)
val onDatatype : Iri.t

(** The property that determines the collection of individuals or data values that build an enumeration. *)
val oneOf : Iri.t

(** The property that determines the n-tuple of properties that a property restriction on an n-ary data range refers to. *)
val onProperties : Iri.t

(** The property that determines the property that a property restriction refers to. *)
val onProperty : Iri.t

(** The class of ontologies. *)
val c_Ontology : Iri.t

(** The class of ontology properties. *)
val c_OntologyProperty : Iri.t

(** The property that determines the n-tuple of properties that build a sub property chain of a given property. *)
val propertyChainAxiom : Iri.t

(** The property that determines that two given properties are disjoint. *)
val propertyDisjointWith : Iri.t

(** The property that determines the cardinality of an exact qualified cardinality restriction. *)
val qualifiedCardinality : Iri.t

(** The class of reflexive properties. *)
val c_ReflexiveProperty : Iri.t

(** The class of property restrictions. *)
val c_Restriction : Iri.t

(** The property that determines that two given individuals are equal. *)
val sameAs : Iri.t

(** The property that determines the class that an existential property restriction refers to. *)
val someValuesFrom : Iri.t

(** The property that determines the subject of a negative property assertion. *)
val sourceIndividual : Iri.t

(** The class of symmetric properties. *)
val c_SymmetricProperty : Iri.t

(** The property that determines the object of a negative object property assertion. *)
val targetIndividual : Iri.t

(** The property that determines the value of a negative data property assertion. *)
val targetValue : Iri.t

(** The class of transitive properties. *)
val c_TransitiveProperty : Iri.t

(** The property that determines the collection of classes or data ranges that build a union. *)
val unionOf : Iri.t

(** The property that determines the collection of facet-value pairs that define a datatype restriction. *)
val withRestrictions : Iri.t


module Open : sig
  (** The class of collections of pairwise different individuals. *)
  val owl_c_AllDifferent : Iri.t

  (** The class of collections of pairwise disjoint classes. *)
  val owl_c_AllDisjointClasses : Iri.t

  (** The class of collections of pairwise disjoint properties. *)
  val owl_c_AllDisjointProperties : Iri.t

  (** The property that determines the class that a universal property restriction refers to. *)
  val owl_allValuesFrom : Iri.t

  (** The property that determines the predicate of an annotated axiom or annotated annotation. *)
  val owl_annotatedProperty : Iri.t

  (** The property that determines the subject of an annotated axiom or annotated annotation. *)
  val owl_annotatedSource : Iri.t

  (** The property that determines the object of an annotated axiom or annotated annotation. *)
  val owl_annotatedTarget : Iri.t

  (** The class of annotated annotations for which the RDF serialization consists of an annotated subject, predicate and object. *)
  val owl_c_Annotation : Iri.t

  (** The class of annotation properties. *)
  val owl_c_AnnotationProperty : Iri.t

  (** The property that determines the predicate of a negative property assertion. *)
  val owl_assertionProperty : Iri.t

  (** The class of asymmetric properties. *)
  val owl_c_AsymmetricProperty : Iri.t

  (** The class of annotated axioms for which the RDF serialization consists of an annotated subject, predicate and object. *)
  val owl_c_Axiom : Iri.t

  (** The property that determines the cardinality of an exact cardinality restriction. *)
  val owl_cardinality : Iri.t

  (** The class of OWL classes. *)
  val owl_c_Class : Iri.t

  (** The property that determines that a given class is the complement of another class. *)
  val owl_complementOf : Iri.t

  (** The class of OWL data ranges, which are special kinds of datatypes. Note: The use of the IRI owl:DataRange has been deprecated as of OWL 2. The IRI rdfs:Datatype SHOULD be used instead. *)
  val owl_c_DataRange : Iri.t

  (** The property that determines that a given data range is the complement of another data range with respect to the data domain. *)
  val owl_datatypeComplementOf : Iri.t

  (** The class of data properties. *)
  val owl_c_DatatypeProperty : Iri.t

  (** The class of deprecated classes. *)
  val owl_c_DeprecatedClass : Iri.t

  (** The class of deprecated properties. *)
  val owl_c_DeprecatedProperty : Iri.t

  (** The property that determines that two given individuals are different. *)
  val owl_differentFrom : Iri.t

  (** The property that determines that a given class is equivalent to the disjoint union of a collection of other classes. *)
  val owl_disjointUnionOf : Iri.t

  (** The property that determines that two given classes are disjoint. *)
  val owl_disjointWith : Iri.t

  (** The property that determines the collection of pairwise different individuals in a owl:AllDifferent axiom. *)
  val owl_distinctMembers : Iri.t

  (** The property that determines that two given classes are equivalent, and that is used to specify datatype definitions. *)
  val owl_equivalentClass : Iri.t

  (** The property that determines that two given properties are equivalent. *)
  val owl_equivalentProperty : Iri.t

  (** The class of functional properties. *)
  val owl_c_FunctionalProperty : Iri.t

  (** The property that determines the collection of properties that jointly build a key. *)
  val owl_hasKey : Iri.t

  (** The property that determines the property that a self restriction refers to. *)
  val owl_hasSelf : Iri.t

  (** The property that determines the individual that a has-value restriction refers to. *)
  val owl_hasValue : Iri.t

  (** The property that determines the collection of classes or data ranges that build an intersection. *)
  val owl_intersectionOf : Iri.t

  (** The class of inverse-functional properties. *)
  val owl_c_InverseFunctionalProperty : Iri.t

  (** The property that determines that two given properties are inverse. *)
  val owl_inverseOf : Iri.t

  (** The class of irreflexive properties. *)
  val owl_c_IrreflexiveProperty : Iri.t

  (** The property that determines the cardinality of a maximum cardinality restriction. *)
  val owl_maxCardinality : Iri.t

  (** The property that determines the cardinality of a maximum qualified cardinality restriction. *)
  val owl_maxQualifiedCardinality : Iri.t

  (** The property that determines the collection of members in either a owl:AllDifferent, owl:AllDisjointClasses or owl:AllDisjointProperties axiom. *)
  val owl_members : Iri.t

  (** The property that determines the cardinality of a minimum cardinality restriction. *)
  val owl_minCardinality : Iri.t

  (** The property that determines the cardinality of a minimum qualified cardinality restriction. *)
  val owl_minQualifiedCardinality : Iri.t

  (** The class of named individuals. *)
  val owl_c_NamedIndividual : Iri.t

  (** The class of negative property assertions. *)
  val owl_c_NegativePropertyAssertion : Iri.t

  (** The class of object properties. *)
  val owl_c_ObjectProperty : Iri.t

  (** The property that determines the class that a qualified object cardinality restriction refers to. *)
  val owl_onClass : Iri.t

  (** The property that determines the data range that a qualified data cardinality restriction refers to. *)
  val owl_onDataRange : Iri.t

  (** The property that determines the datatype that a datatype restriction refers to. *)
  val owl_onDatatype : Iri.t

  (** The property that determines the collection of individuals or data values that build an enumeration. *)
  val owl_oneOf : Iri.t

  (** The property that determines the n-tuple of properties that a property restriction on an n-ary data range refers to. *)
  val owl_onProperties : Iri.t

  (** The property that determines the property that a property restriction refers to. *)
  val owl_onProperty : Iri.t

  (** The class of ontologies. *)
  val owl_c_Ontology : Iri.t

  (** The class of ontology properties. *)
  val owl_c_OntologyProperty : Iri.t

  (** The property that determines the n-tuple of properties that build a sub property chain of a given property. *)
  val owl_propertyChainAxiom : Iri.t

  (** The property that determines that two given properties are disjoint. *)
  val owl_propertyDisjointWith : Iri.t

  (** The property that determines the cardinality of an exact qualified cardinality restriction. *)
  val owl_qualifiedCardinality : Iri.t

  (** The class of reflexive properties. *)
  val owl_c_ReflexiveProperty : Iri.t

  (** The class of property restrictions. *)
  val owl_c_Restriction : Iri.t

  (** The property that determines that two given individuals are equal. *)
  val owl_sameAs : Iri.t

  (** The property that determines the class that an existential property restriction refers to. *)
  val owl_someValuesFrom : Iri.t

  (** The property that determines the subject of a negative property assertion. *)
  val owl_sourceIndividual : Iri.t

  (** The class of symmetric properties. *)
  val owl_c_SymmetricProperty : Iri.t

  (** The property that determines the object of a negative object property assertion. *)
  val owl_targetIndividual : Iri.t

  (** The property that determines the value of a negative data property assertion. *)
  val owl_targetValue : Iri.t

  (** The class of transitive properties. *)
  val owl_c_TransitiveProperty : Iri.t

  (** The property that determines the collection of classes or data ranges that build a union. *)
  val owl_unionOf : Iri.t

  (** The property that determines the collection of facet-value pairs that define a datatype restriction. *)
  val owl_withRestrictions : Iri.t

end

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

(** The class of annotated annotations for which the RDF serialization consists of an annotated subject, predicate and object. *)
val c_Annotation : Iri.t

(** The class of annotation properties. *)
val c_AnnotationProperty : Iri.t

(** The class of asymmetric properties. *)
val c_AsymmetricProperty : Iri.t

(** The class of annotated axioms for which the RDF serialization consists of an annotated subject, predicate and object. *)
val c_Axiom : Iri.t

(** The class of OWL classes. *)
val c_Class : Iri.t

(** The class of OWL data ranges, which are special kinds of datatypes. Note: The use of the IRI owl:DataRange has been deprecated as of OWL 2. The IRI rdfs:Datatype SHOULD be used instead. *)
val c_DataRange : Iri.t

(** The class of data properties. *)
val c_DatatypeProperty : Iri.t

(** The class of deprecated classes. *)
val c_DeprecatedClass : Iri.t

(** The class of deprecated properties. *)
val c_DeprecatedProperty : Iri.t

(** The class of functional properties. *)
val c_FunctionalProperty : Iri.t

(** The class of inverse-functional properties. *)
val c_InverseFunctionalProperty : Iri.t

(** The class of irreflexive properties. *)
val c_IrreflexiveProperty : Iri.t

(** The class of named individuals. *)
val c_NamedIndividual : Iri.t

(** The class of negative property assertions. *)
val c_NegativePropertyAssertion : Iri.t

(** This is the empty class. *)
val c_Nothing : Iri.t

(** The class of object properties. *)
val c_ObjectProperty : Iri.t

(** The class of ontologies. *)
val c_Ontology : Iri.t

(** The class of ontology properties. *)
val c_OntologyProperty : Iri.t

(** The class of reflexive properties. *)
val c_ReflexiveProperty : Iri.t

(** The class of property restrictions. *)
val c_Restriction : Iri.t

(** The class of symmetric properties. *)
val c_SymmetricProperty : Iri.t

(** The class of OWL individuals. *)
val c_Thing : Iri.t

(** The class of transitive properties. *)
val c_TransitiveProperty : Iri.t

(** The property that determines the class that a universal property restriction refers to. *)
val allValuesFrom : Iri.t

(** The property that determines the predicate of an annotated axiom or annotated annotation. *)
val annotatedProperty : Iri.t

(** The property that determines the subject of an annotated axiom or annotated annotation. *)
val annotatedSource : Iri.t

(** The property that determines the object of an annotated axiom or annotated annotation. *)
val annotatedTarget : Iri.t

(** The property that determines the predicate of a negative property assertion. *)
val assertionProperty : Iri.t

(** The annotation property that indicates that a given ontology is backward compatible with another ontology. *)
val backwardCompatibleWith : Iri.t

(** The data property that does not relate any individual to any data value. *)
val bottomDataProperty : Iri.t

(** The object property that does not relate any two individuals. *)
val bottomObjectProperty : Iri.t

(** The property that determines the cardinality of an exact cardinality restriction. *)
val cardinality : Iri.t

(** The property that determines that a given class is the complement of another class. *)
val complementOf : Iri.t

(** The property that determines that a given data range is the complement of another data range with respect to the data domain. *)
val datatypeComplementOf : Iri.t

(** The annotation property that indicates that a given entity has been deprecated. *)
val deprecated : Iri.t

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

(** The property that determines the collection of properties that jointly build a key. *)
val hasKey : Iri.t

(** The property that determines the property that a self restriction refers to. *)
val hasSelf : Iri.t

(** The property that determines the individual that a has-value restriction refers to. *)
val hasValue : Iri.t

(** The annotation property that indicates that a given ontology is incompatible with another ontology. *)
val incompatibleWith : Iri.t

(** The property that determines the collection of classes or data ranges that build an intersection. *)
val intersectionOf : Iri.t

(** The property that determines that two given properties are inverse. *)
val inverseOf : Iri.t

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

(** The property that determines the class that a qualified object cardinality restriction refers to. *)
val onClass : Iri.t

(** The property that determines the data range that a qualified data cardinality restriction refers to. *)
val onDataRange : Iri.t

(** The property that determines the datatype that a datatype restriction refers to. *)
val onDatatype : Iri.t

(** The property that determines the n-tuple of properties that a property restriction on an n-ary data range refers to. *)
val onProperties : Iri.t

(** The property that determines the property that a property restriction refers to. *)
val onProperty : Iri.t

(** The property that determines the collection of individuals or data values that build an enumeration. *)
val oneOf : Iri.t

(** The annotation property that indicates the predecessor ontology of a given ontology. *)
val priorVersion : Iri.t

(** The property that determines the n-tuple of properties that build a sub property chain of a given property. *)
val propertyChainAxiom : Iri.t

(** The property that determines that two given properties are disjoint. *)
val propertyDisjointWith : Iri.t

(** The property that determines the cardinality of an exact qualified cardinality restriction. *)
val qualifiedCardinality : Iri.t

(** The property that determines that two given individuals are equal. *)
val sameAs : Iri.t

(** The property that determines the class that an existential property restriction refers to. *)
val someValuesFrom : Iri.t

(** The property that determines the subject of a negative property assertion. *)
val sourceIndividual : Iri.t

(** The property that determines the object of a negative object property assertion. *)
val targetIndividual : Iri.t

(** The property that determines the value of a negative data property assertion. *)
val targetValue : Iri.t

(** The data property that relates every individual to every data value. *)
val topDataProperty : Iri.t

(** The object property that relates every two individuals. *)
val topObjectProperty : Iri.t

(** The property that determines the collection of classes or data ranges that build a union. *)
val unionOf : Iri.t

(** The annotation property that provides version information for an ontology or another OWL construct. *)
val versionInfo : Iri.t

(** The property that determines the collection of facet-value pairs that define a datatype restriction. *)
val withRestrictions : Iri.t


module Open : sig
  (** The class of collections of pairwise different individuals. *)
  val owl_c_AllDifferent : Iri.t

  (** The class of collections of pairwise disjoint classes. *)
  val owl_c_AllDisjointClasses : Iri.t

  (** The class of collections of pairwise disjoint properties. *)
  val owl_c_AllDisjointProperties : Iri.t

  (** The class of annotated annotations for which the RDF serialization consists of an annotated subject, predicate and object. *)
  val owl_c_Annotation : Iri.t

  (** The class of annotation properties. *)
  val owl_c_AnnotationProperty : Iri.t

  (** The class of asymmetric properties. *)
  val owl_c_AsymmetricProperty : Iri.t

  (** The class of annotated axioms for which the RDF serialization consists of an annotated subject, predicate and object. *)
  val owl_c_Axiom : Iri.t

  (** The class of OWL classes. *)
  val owl_c_Class : Iri.t

  (** The class of OWL data ranges, which are special kinds of datatypes. Note: The use of the IRI owl:DataRange has been deprecated as of OWL 2. The IRI rdfs:Datatype SHOULD be used instead. *)
  val owl_c_DataRange : Iri.t

  (** The class of data properties. *)
  val owl_c_DatatypeProperty : Iri.t

  (** The class of deprecated classes. *)
  val owl_c_DeprecatedClass : Iri.t

  (** The class of deprecated properties. *)
  val owl_c_DeprecatedProperty : Iri.t

  (** The class of functional properties. *)
  val owl_c_FunctionalProperty : Iri.t

  (** The class of inverse-functional properties. *)
  val owl_c_InverseFunctionalProperty : Iri.t

  (** The class of irreflexive properties. *)
  val owl_c_IrreflexiveProperty : Iri.t

  (** The class of named individuals. *)
  val owl_c_NamedIndividual : Iri.t

  (** The class of negative property assertions. *)
  val owl_c_NegativePropertyAssertion : Iri.t

  (** This is the empty class. *)
  val owl_c_Nothing : Iri.t

  (** The class of object properties. *)
  val owl_c_ObjectProperty : Iri.t

  (** The class of ontologies. *)
  val owl_c_Ontology : Iri.t

  (** The class of ontology properties. *)
  val owl_c_OntologyProperty : Iri.t

  (** The class of reflexive properties. *)
  val owl_c_ReflexiveProperty : Iri.t

  (** The class of property restrictions. *)
  val owl_c_Restriction : Iri.t

  (** The class of symmetric properties. *)
  val owl_c_SymmetricProperty : Iri.t

  (** The class of OWL individuals. *)
  val owl_c_Thing : Iri.t

  (** The class of transitive properties. *)
  val owl_c_TransitiveProperty : Iri.t

  (** The property that determines the class that a universal property restriction refers to. *)
  val owl_allValuesFrom : Iri.t

  (** The property that determines the predicate of an annotated axiom or annotated annotation. *)
  val owl_annotatedProperty : Iri.t

  (** The property that determines the subject of an annotated axiom or annotated annotation. *)
  val owl_annotatedSource : Iri.t

  (** The property that determines the object of an annotated axiom or annotated annotation. *)
  val owl_annotatedTarget : Iri.t

  (** The property that determines the predicate of a negative property assertion. *)
  val owl_assertionProperty : Iri.t

  (** The annotation property that indicates that a given ontology is backward compatible with another ontology. *)
  val owl_backwardCompatibleWith : Iri.t

  (** The data property that does not relate any individual to any data value. *)
  val owl_bottomDataProperty : Iri.t

  (** The object property that does not relate any two individuals. *)
  val owl_bottomObjectProperty : Iri.t

  (** The property that determines the cardinality of an exact cardinality restriction. *)
  val owl_cardinality : Iri.t

  (** The property that determines that a given class is the complement of another class. *)
  val owl_complementOf : Iri.t

  (** The property that determines that a given data range is the complement of another data range with respect to the data domain. *)
  val owl_datatypeComplementOf : Iri.t

  (** The annotation property that indicates that a given entity has been deprecated. *)
  val owl_deprecated : Iri.t

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

  (** The property that determines the collection of properties that jointly build a key. *)
  val owl_hasKey : Iri.t

  (** The property that determines the property that a self restriction refers to. *)
  val owl_hasSelf : Iri.t

  (** The property that determines the individual that a has-value restriction refers to. *)
  val owl_hasValue : Iri.t

  (** The annotation property that indicates that a given ontology is incompatible with another ontology. *)
  val owl_incompatibleWith : Iri.t

  (** The property that determines the collection of classes or data ranges that build an intersection. *)
  val owl_intersectionOf : Iri.t

  (** The property that determines that two given properties are inverse. *)
  val owl_inverseOf : Iri.t

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

  (** The property that determines the class that a qualified object cardinality restriction refers to. *)
  val owl_onClass : Iri.t

  (** The property that determines the data range that a qualified data cardinality restriction refers to. *)
  val owl_onDataRange : Iri.t

  (** The property that determines the datatype that a datatype restriction refers to. *)
  val owl_onDatatype : Iri.t

  (** The property that determines the n-tuple of properties that a property restriction on an n-ary data range refers to. *)
  val owl_onProperties : Iri.t

  (** The property that determines the property that a property restriction refers to. *)
  val owl_onProperty : Iri.t

  (** The property that determines the collection of individuals or data values that build an enumeration. *)
  val owl_oneOf : Iri.t

  (** The annotation property that indicates the predecessor ontology of a given ontology. *)
  val owl_priorVersion : Iri.t

  (** The property that determines the n-tuple of properties that build a sub property chain of a given property. *)
  val owl_propertyChainAxiom : Iri.t

  (** The property that determines that two given properties are disjoint. *)
  val owl_propertyDisjointWith : Iri.t

  (** The property that determines the cardinality of an exact qualified cardinality restriction. *)
  val owl_qualifiedCardinality : Iri.t

  (** The property that determines that two given individuals are equal. *)
  val owl_sameAs : Iri.t

  (** The property that determines the class that an existential property restriction refers to. *)
  val owl_someValuesFrom : Iri.t

  (** The property that determines the subject of a negative property assertion. *)
  val owl_sourceIndividual : Iri.t

  (** The property that determines the object of a negative object property assertion. *)
  val owl_targetIndividual : Iri.t

  (** The property that determines the value of a negative data property assertion. *)
  val owl_targetValue : Iri.t

  (** The data property that relates every individual to every data value. *)
  val owl_topDataProperty : Iri.t

  (** The object property that relates every two individuals. *)
  val owl_topObjectProperty : Iri.t

  (** The property that determines the collection of classes or data ranges that build a union. *)
  val owl_unionOf : Iri.t

  (** The annotation property that provides version information for an ontology or another OWL construct. *)
  val owl_versionInfo : Iri.t

  (** The property that determines the collection of facet-value pairs that define a datatype restriction. *)
  val owl_withRestrictions : Iri.t

end

class from : ?sub: Iri.t -> Rdf_graph.graph ->
  object
    method allValuesFrom : Iri.t list
    method annotatedProperty : Iri.t list
    method annotatedSource : Iri.t list
    method annotatedTarget : Iri.t list
    method assertionProperty : Iri.t list
    method backwardCompatibleWith : Iri.t list
    method bottomDataProperty : Rdf_term.literal list
    method bottomObjectProperty : Iri.t list
    method cardinality : Iri.t list
    method complementOf : Iri.t list
    method datatypeComplementOf : Iri.t list
    method deprecated : Iri.t list
    method differentFrom : Iri.t list
    method disjointUnionOf : Iri.t list
    method disjointWith : Iri.t list
    method distinctMembers : Iri.t list
    method equivalentClass : Iri.t list
    method equivalentProperty : Iri.t list
    method hasKey : Iri.t list
    method hasSelf : Iri.t list
    method hasValue : Iri.t list
    method incompatibleWith : Iri.t list
    method intersectionOf : Iri.t list
    method inverseOf : Iri.t list
    method maxCardinality : Iri.t list
    method maxQualifiedCardinality : Iri.t list
    method members : Iri.t list
    method minCardinality : Iri.t list
    method minQualifiedCardinality : Iri.t list
    method onClass : Iri.t list
    method onDataRange : Iri.t list
    method onDatatype : Iri.t list
    method onProperties : Iri.t list
    method onProperty : Iri.t list
    method oneOf : Iri.t list
    method priorVersion : Iri.t list
    method propertyChainAxiom : Iri.t list
    method propertyDisjointWith : Iri.t list
    method qualifiedCardinality : Iri.t list
    method sameAs : Iri.t list
    method someValuesFrom : Iri.t list
    method sourceIndividual : Iri.t list
    method targetIndividual : Iri.t list
    method targetValue : Rdf_term.literal list
    method topDataProperty : Rdf_term.literal list
    method topObjectProperty : Iri.t list
    method unionOf : Iri.t list
    method versionInfo : Iri.t list
    method withRestrictions : Iri.t list
  end

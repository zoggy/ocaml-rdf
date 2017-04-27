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
    method allValuesFrom : Rdf_term.term list
    method allValuesFrom_opt : Rdf_term.term option
    method allValuesFrom_iris : Iri.t list
    method allValuesFrom_opt_iri : Iri.t option
    method annotatedProperty : Rdf_term.term list
    method annotatedProperty_opt : Rdf_term.term option
    method annotatedProperty_iris : Iri.t list
    method annotatedProperty_opt_iri : Iri.t option
    method annotatedSource : Rdf_term.term list
    method annotatedSource_opt : Rdf_term.term option
    method annotatedSource_iris : Iri.t list
    method annotatedSource_opt_iri : Iri.t option
    method annotatedTarget : Rdf_term.term list
    method annotatedTarget_opt : Rdf_term.term option
    method annotatedTarget_iris : Iri.t list
    method annotatedTarget_opt_iri : Iri.t option
    method assertionProperty : Rdf_term.term list
    method assertionProperty_opt : Rdf_term.term option
    method assertionProperty_iris : Iri.t list
    method assertionProperty_opt_iri : Iri.t option
    method backwardCompatibleWith : Rdf_term.term list
    method backwardCompatibleWith_opt : Rdf_term.term option
    method backwardCompatibleWith_iris : Iri.t list
    method backwardCompatibleWith_opt_iri : Iri.t option
    method bottomDataProperty : Rdf_term.literal list
    method bottomDataProperty_opt : Rdf_term.literal option
    method bottomObjectProperty : Rdf_term.term list
    method bottomObjectProperty_opt : Rdf_term.term option
    method bottomObjectProperty_iris : Iri.t list
    method bottomObjectProperty_opt_iri : Iri.t option
    method cardinality : Rdf_term.term list
    method cardinality_opt : Rdf_term.term option
    method cardinality_iris : Iri.t list
    method cardinality_opt_iri : Iri.t option
    method complementOf : Rdf_term.term list
    method complementOf_opt : Rdf_term.term option
    method complementOf_iris : Iri.t list
    method complementOf_opt_iri : Iri.t option
    method datatypeComplementOf : Rdf_term.literal list
    method datatypeComplementOf_opt : Rdf_term.literal option
    method deprecated : Rdf_term.term list
    method deprecated_opt : Rdf_term.term option
    method deprecated_iris : Iri.t list
    method deprecated_opt_iri : Iri.t option
    method differentFrom : Rdf_term.term list
    method differentFrom_opt : Rdf_term.term option
    method differentFrom_iris : Iri.t list
    method differentFrom_opt_iri : Iri.t option
    method disjointUnionOf : Rdf_term.term list
    method disjointUnionOf_opt : Rdf_term.term option
    method disjointUnionOf_iris : Iri.t list
    method disjointUnionOf_opt_iri : Iri.t option
    method disjointWith : Rdf_term.term list
    method disjointWith_opt : Rdf_term.term option
    method disjointWith_iris : Iri.t list
    method disjointWith_opt_iri : Iri.t option
    method distinctMembers : Rdf_term.term list
    method distinctMembers_opt : Rdf_term.term option
    method distinctMembers_iris : Iri.t list
    method distinctMembers_opt_iri : Iri.t option
    method equivalentClass : Rdf_term.term list
    method equivalentClass_opt : Rdf_term.term option
    method equivalentClass_iris : Iri.t list
    method equivalentClass_opt_iri : Iri.t option
    method equivalentProperty : Rdf_term.term list
    method equivalentProperty_opt : Rdf_term.term option
    method equivalentProperty_iris : Iri.t list
    method equivalentProperty_opt_iri : Iri.t option
    method hasKey : Rdf_term.term list
    method hasKey_opt : Rdf_term.term option
    method hasKey_iris : Iri.t list
    method hasKey_opt_iri : Iri.t option
    method hasSelf : Rdf_term.term list
    method hasSelf_opt : Rdf_term.term option
    method hasSelf_iris : Iri.t list
    method hasSelf_opt_iri : Iri.t option
    method hasValue : Rdf_term.term list
    method hasValue_opt : Rdf_term.term option
    method hasValue_iris : Iri.t list
    method hasValue_opt_iri : Iri.t option
    method incompatibleWith : Rdf_term.term list
    method incompatibleWith_opt : Rdf_term.term option
    method incompatibleWith_iris : Iri.t list
    method incompatibleWith_opt_iri : Iri.t option
    method intersectionOf : Rdf_term.term list
    method intersectionOf_opt : Rdf_term.term option
    method intersectionOf_iris : Iri.t list
    method intersectionOf_opt_iri : Iri.t option
    method inverseOf : Rdf_term.term list
    method inverseOf_opt : Rdf_term.term option
    method inverseOf_iris : Iri.t list
    method inverseOf_opt_iri : Iri.t option
    method maxCardinality : Rdf_term.term list
    method maxCardinality_opt : Rdf_term.term option
    method maxCardinality_iris : Iri.t list
    method maxCardinality_opt_iri : Iri.t option
    method maxQualifiedCardinality : Rdf_term.term list
    method maxQualifiedCardinality_opt : Rdf_term.term option
    method maxQualifiedCardinality_iris : Iri.t list
    method maxQualifiedCardinality_opt_iri : Iri.t option
    method members : Rdf_term.term list
    method members_opt : Rdf_term.term option
    method members_iris : Iri.t list
    method members_opt_iri : Iri.t option
    method minCardinality : Rdf_term.term list
    method minCardinality_opt : Rdf_term.term option
    method minCardinality_iris : Iri.t list
    method minCardinality_opt_iri : Iri.t option
    method minQualifiedCardinality : Rdf_term.term list
    method minQualifiedCardinality_opt : Rdf_term.term option
    method minQualifiedCardinality_iris : Iri.t list
    method minQualifiedCardinality_opt_iri : Iri.t option
    method onClass : Rdf_term.term list
    method onClass_opt : Rdf_term.term option
    method onClass_iris : Iri.t list
    method onClass_opt_iri : Iri.t option
    method onDataRange : Rdf_term.literal list
    method onDataRange_opt : Rdf_term.literal option
    method onDatatype : Rdf_term.literal list
    method onDatatype_opt : Rdf_term.literal option
    method onProperties : Rdf_term.term list
    method onProperties_opt : Rdf_term.term option
    method onProperties_iris : Iri.t list
    method onProperties_opt_iri : Iri.t option
    method onProperty : Rdf_term.term list
    method onProperty_opt : Rdf_term.term option
    method onProperty_iris : Iri.t list
    method onProperty_opt_iri : Iri.t option
    method oneOf : Rdf_term.term list
    method oneOf_opt : Rdf_term.term option
    method oneOf_iris : Iri.t list
    method oneOf_opt_iri : Iri.t option
    method priorVersion : Rdf_term.term list
    method priorVersion_opt : Rdf_term.term option
    method priorVersion_iris : Iri.t list
    method priorVersion_opt_iri : Iri.t option
    method propertyChainAxiom : Rdf_term.term list
    method propertyChainAxiom_opt : Rdf_term.term option
    method propertyChainAxiom_iris : Iri.t list
    method propertyChainAxiom_opt_iri : Iri.t option
    method propertyDisjointWith : Rdf_term.term list
    method propertyDisjointWith_opt : Rdf_term.term option
    method propertyDisjointWith_iris : Iri.t list
    method propertyDisjointWith_opt_iri : Iri.t option
    method qualifiedCardinality : Rdf_term.term list
    method qualifiedCardinality_opt : Rdf_term.term option
    method qualifiedCardinality_iris : Iri.t list
    method qualifiedCardinality_opt_iri : Iri.t option
    method sameAs : Rdf_term.term list
    method sameAs_opt : Rdf_term.term option
    method sameAs_iris : Iri.t list
    method sameAs_opt_iri : Iri.t option
    method someValuesFrom : Rdf_term.term list
    method someValuesFrom_opt : Rdf_term.term option
    method someValuesFrom_iris : Iri.t list
    method someValuesFrom_opt_iri : Iri.t option
    method sourceIndividual : Rdf_term.term list
    method sourceIndividual_opt : Rdf_term.term option
    method sourceIndividual_iris : Iri.t list
    method sourceIndividual_opt_iri : Iri.t option
    method targetIndividual : Rdf_term.term list
    method targetIndividual_opt : Rdf_term.term option
    method targetIndividual_iris : Iri.t list
    method targetIndividual_opt_iri : Iri.t option
    method targetValue : Rdf_term.literal list
    method targetValue_opt : Rdf_term.literal option
    method topDataProperty : Rdf_term.literal list
    method topDataProperty_opt : Rdf_term.literal option
    method topObjectProperty : Rdf_term.term list
    method topObjectProperty_opt : Rdf_term.term option
    method topObjectProperty_iris : Iri.t list
    method topObjectProperty_opt_iri : Iri.t option
    method unionOf : Rdf_term.term list
    method unionOf_opt : Rdf_term.term option
    method unionOf_iris : Iri.t list
    method unionOf_opt_iri : Iri.t option
    method versionInfo : Rdf_term.term list
    method versionInfo_opt : Rdf_term.term option
    method versionInfo_iris : Iri.t list
    method versionInfo_opt_iri : Iri.t option
    method withRestrictions : Rdf_term.term list
    method withRestrictions_opt : Rdf_term.term option
    method withRestrictions_iris : Iri.t list
    method withRestrictions_opt_iri : Iri.t option
  end

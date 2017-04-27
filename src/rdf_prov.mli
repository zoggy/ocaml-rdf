(** Elements of [http://www.w3.org/ns/prov#] *)

(** [http://www.w3.org/ns/prov#] *)
val prov : Iri.t
val prov_ : string -> Iri.t

val c_Accept : Iri.t

val c_Activity : Iri.t

(** It is not recommended that the type ActivityInfluence be asserted without also asserting one of its more specific subclasses. *)
val c_ActivityInfluence : Iri.t

val c_Agent : Iri.t

(** It is not recommended that the type AgentInfluence be asserted without also asserting one of its more specific subclasses. *)
val c_AgentInfluence : Iri.t

(** An instance of prov:Association provides additional descriptions about the binary prov:wasAssociatedWith relation from an prov:Activity to some prov:Agent that had some responsiblity for it. For example, :baking prov:wasAssociatedWith :baker; prov:qualifiedAssociation [ a prov:Association; prov:agent :baker; :foo :bar ]. *)
val c_Association : Iri.t

(** An instance of prov:Attribution provides additional descriptions about the binary prov:wasAttributedTo relation from an prov:Entity to some prov:Agent that had some responsible for it. For example, :cake prov:wasAttributedTo :baker; prov:qualifiedAttribution [ a prov:Attribution; prov:entity :baker; :foo :bar ]. *)
val c_Attribution : Iri.t

(** Note that there are kinds of bundles (e.g. handwritten letters, audio recordings, etc.) that are not expressed in PROV-O, but can be still be described by PROV-O. *)
val c_Bundle : Iri.t

val c_Collection : Iri.t

(** An instance of prov:Communication provides additional descriptions about the binary prov:wasInformedBy relation from an informed prov:Activity to the prov:Activity that informed it. For example, :you_jumping_off_bridge prov:wasInformedBy :everyone_else_jumping_off_bridge; prov:qualifiedCommunication [ a prov:Communication; prov:activity :everyone_else_jumping_off_bridge; :foo :bar ]. *)
val c_Communication : Iri.t

val c_Contribute : Iri.t

val c_Contributor : Iri.t

val c_Copyright : Iri.t

val c_Create : Iri.t

val c_Creator : Iri.t

(** An instance of prov:Delegation provides additional descriptions about the binary prov:actedOnBehalfOf relation from a performing prov:Agent to some prov:Agent for whom it was performed. For example, :mixing prov:wasAssociatedWith :toddler . :toddler prov:actedOnBehalfOf :mother; prov:qualifiedDelegation [ a prov:Delegation; prov:entity :mother; :foo :bar ]. *)
val c_Delegation : Iri.t

(** The more specific forms of prov:Derivation (i.e., prov:Revision, prov:Quotation, prov:PrimarySource) should be asserted if they apply. *)
val c_Derivation : Iri.t

(** This concept allows for the provenance of the dictionary, but also of its constituents to be expressed. Such a notion of dictionary corresponds to a wide variety of concrete data structures, such as a maps or associative arrays. *)
val c_Dictionary : Iri.t

(** Type for a generic provenance query service. Mainly for use in RDF provenance query service descriptions, to facilitate discovery in linked data environments. *)
val c_DirectQueryService : Iri.t

val c_EmptyCollection : Iri.t

val c_EmptyDictionary : Iri.t

(** An instance of prov:End provides additional descriptions about the binary prov:wasEndedBy relation from some ended prov:Activity to an prov:Entity that ended it. For example, :ball_game prov:wasEndedBy :buzzer; prov:qualifiedEnd [ a prov:End; prov:entity :buzzer; :foo :bar; prov:atTime '2012-03-09T08:05:08-05:00'^^xsd:dateTime ]. *)
val c_End : Iri.t

val c_Entity : Iri.t

(** It is not recommended that the type EntityInfluence be asserted without also asserting one of its more specific subclasses. *)
val c_EntityInfluence : Iri.t

(** An instance of prov:Generation provides additional descriptions about the binary prov:wasGeneratedBy relation from a generated prov:Entity to the prov:Activity that generated it. For example, :cake prov:wasGeneratedBy :baking; prov:qualifiedGeneration [ a prov:Generation; prov:activity :baking; :foo :bar ]. *)
val c_Generation : Iri.t

(** Because prov:Influence is a broad relation, its most specific subclasses (e.g. prov:Communication, prov:Delegation, prov:End, prov:Revision, etc.) should be used when applicable. *)
val c_Influence : Iri.t

val c_Insertion : Iri.t

(** An instantaneous event, or event for short, happens in the world and marks a change in the world, in its activities and in its entities. The term 'event' is commonly used in process algebra with a similar meaning. Events represent communications or interactions; they are assumed to be atomic and instantaneous. *)
val c_InstantaneousEvent : Iri.t

(** An instance of prov:Invalidation provides additional descriptions about the binary prov:wasInvalidatedBy relation from an invalidated prov:Entity to the prov:Activity that invalidated it. For example, :uncracked_egg prov:wasInvalidatedBy :baking; prov:qualifiedInvalidation [ a prov:Invalidation; prov:activity :baking; :foo :bar ]. *)
val c_Invalidation : Iri.t

val c_KeyEntityPair : Iri.t

val c_Location : Iri.t

val c_Modify : Iri.t

val c_Organization : Iri.t

val c_Person : Iri.t

(** There exist no prescriptive requirement on the nature of plans, their representation, the actions or steps they consist of, or their intended goals. Since plans may evolve over time, it may become necessary to track their provenance, so plans themselves are entities. Representing the plan explicitly in the provenance can be useful for various tasks: for example, to validate the execution as represented in the provenance record, to manage expectation failures, or to provide explanations. *)
val c_Plan : Iri.t

(** An instance of prov:PrimarySource provides additional descriptions about the binary prov:hadPrimarySource relation from some secondary prov:Entity to an earlier, primary prov:Entity. For example, :blog prov:hadPrimarySource :newsArticle; prov:qualifiedPrimarySource [ a prov:PrimarySource; prov:entity :newsArticle; :foo :bar ] . *)
val c_PrimarySource : Iri.t

val c_Publish : Iri.t

val c_Publisher : Iri.t

(** An instance of prov:Quotation provides additional descriptions about the binary prov:wasQuotedFrom relation from some taken prov:Entity from an earlier, larger prov:Entity. For example, :here_is_looking_at_you_kid prov:wasQuotedFrom :casablanca_script; prov:qualifiedQuotation [ a prov:Quotation; prov:entity :casablanca_script; :foo :bar ]. *)
val c_Quotation : Iri.t

val c_Removal : Iri.t

val c_Replace : Iri.t

(** An instance of prov:Revision provides additional descriptions about the binary prov:wasRevisionOf relation from some newer prov:Entity to an earlier prov:Entity. For example, :draft_2 prov:wasRevisionOf :draft_1; prov:qualifiedRevision [ a prov:Revision; prov:entity :draft_1; :foo :bar ]. *)
val c_Revision : Iri.t

val c_RightsAssignment : Iri.t

val c_RightsHolder : Iri.t

val c_Role : Iri.t

(** Type for a generic provenance query service. Mainly for use in RDF provenance query service descriptions, to facilitate discovery in linked data environments. *)
val c_ServiceDescription : Iri.t

val c_SoftwareAgent : Iri.t

(** An instance of prov:Start provides additional descriptions about the binary prov:wasStartedBy relation from some started prov:Activity to an prov:Entity that started it. For example, :foot_race prov:wasStartedBy :bang; prov:qualifiedStart [ a prov:Start; prov:entity :bang; :foo :bar; prov:atTime '2012-03-09T08:05:08-05:00'^^xsd:dateTime ] . *)
val c_Start : Iri.t

val c_Submit : Iri.t

(** An instance of prov:Usage provides additional descriptions about the binary prov:used relation from some prov:Activity to an prov:Entity that it used. For example, :keynote prov:used :podium; prov:qualifiedUsage [ a prov:Usage; prov:entity :podium; :foo :bar ]. *)
val c_Usage : Iri.t

(** An object property to express the accountability of an agent towards another agent. The subordinate agent acted on behalf of the responsible agent in an actual activity.  *)
val actedOnBehalfOf : Iri.t

val activity : Iri.t

val agent : Iri.t

val alternateOf : Iri.t

val aq : Iri.t

(** prov:asInBundle is used to specify which bundle the general entity of a prov:mentionOf property is described.

When :x prov:mentionOf :y and :y is described in Bundle :b, the triple :x prov:asInBundle :b is also asserted to cite the Bundle in which :y was described. *)
val asInBundle : Iri.t

(** The Location of any resource. *)
val atLocation : Iri.t

(** The time at which an InstantaneousEvent occurred, in the form of xsd:dateTime. *)
val atTime : Iri.t

(** Classify prov-o terms into three categories, including 'starting-point', 'qualifed', and 'extended'. This classification is used by the prov-o html document to gently introduce prov-o terms to its users.  *)
val category : Iri.t

(** Classify prov-o terms into six components according to prov-dm, including 'agents-responsibility', 'alternate', 'annotations', 'collections', 'derivations', and 'entities-activities'. This classification is used so that readers of prov-o specification can find its correspondence with the prov-dm specification. *)
val component : Iri.t

(** A reference to the principal section of the PROV-CONSTRAINTS document that describes this concept. *)
val constraints : Iri.t

(** A definition quoted from PROV-DM or PROV-CONSTRAINTS that describes the concept expressed with this OWL term. *)
val definition : Iri.t

val derivedByInsertionFrom : Iri.t

val derivedByRemovalFrom : Iri.t

(** relates a generic provenance query service resource (type prov:ServiceDescription) to a specific query service description (e.g. a prov:DirectQueryService or a sd:Service). *)
val describesService : Iri.t

val dictionary : Iri.t

(** A reference to the principal section of the PROV-DM document that describes this concept. *)
val dm : Iri.t

(** A note by the OWL development team about how this term expresses the PROV-DM concept, or how it should be used in context of semantic web or linked data. *)
val editorialNote : Iri.t

(** When the prov-o term does not have a definition drawn from prov-dm, and the prov-o editor provides one. *)
val editorsDefinition : Iri.t

(** The time at which an activity ended. See also prov:startedAtTime. *)
val endedAtTime : Iri.t

val entity : Iri.t

val generated : Iri.t

(** The time at which an entity was completely created and is available for use. *)
val generatedAtTime : Iri.t

(** The _optional_ Activity of an Influence, which used, generated, invalidated, or was the responsibility of some Entity. This property is _not_ used by ActivityInfluence (use prov:activity instead). *)
val hadActivity : Iri.t

val hadDictionaryMember : Iri.t

(** The _optional_ Generation involved in an Entity's Derivation. *)
val hadGeneration : Iri.t

val hadMember : Iri.t

(** The _optional_ Plan adopted by an Agent in Association with some Activity. Plan specifications are out of the scope of this specification. *)
val hadPlan : Iri.t

val hadPrimarySource : Iri.t

(** The _optional_ Role that an Entity assumed in the context of an Activity. For example, :baking prov:used :spoon; prov:qualified [ a prov:Usage; prov:entity :spoon; prov:hadRole roles:mixing_implement ]. *)
val hadRole : Iri.t

(** The _optional_ Usage involved in an Entity's Derivation. *)
val hadUsage : Iri.t

(** Indicates anchor URI for a potentially dynamic resource instance. *)
val has_anchor : Iri.t

(** Indicates a provenance-URI for a resource; the resource identified by this property presents a provenance record about its subject or anchor resource. *)
val has_provenance : Iri.t

(** Indicates a provenance query service that can access provenance related to its subject or anchor resource. *)
val has_query_service : Iri.t

val influenced : Iri.t

(** Subproperties of prov:influencer are used to cite the object of an unqualified PROV-O triple whose predicate is a subproperty of prov:wasInfluencedBy (e.g. prov:used, prov:wasGeneratedBy). prov:influencer is used much like rdf:object is used. *)
val influencer : Iri.t

val insertedKeyEntityPair : Iri.t

val invalidated : Iri.t

(** The time at which an entity was invalidated (i.e., no longer usable). *)
val invalidatedAtTime : Iri.t

(** PROV-O does not define all property inverses. The directionalities defined in PROV-O should be given preference over those not defined. However, if users wish to name the inverse of a PROV-O property, the local name given by prov:inverse should be used. *)
val inverse : Iri.t

(** prov:mentionOf is used to specialize an entity as described in another bundle. It is to be used in conjuction with prov:asInBundle.

prov:asInBundle is used to cite the Bundle in which the generalization was mentioned. *)
val mentionOf : Iri.t

(** A reference to the principal section of the PROV-M document that describes this concept. *)
val n : Iri.t

(** The position that this OWL term should be listed within documentation. The scope of the documentation (e.g., among all terms, among terms within a prov:category, among properties applying to a particular class, etc.) is unspecified. *)
val order : Iri.t

val pairEntity : Iri.t

val pairKey : Iri.t

(** Relates a resource to a provenance pingback service that may receive additional provenance links about the resource. *)
val pingback : Iri.t

(** Relates a provenance service to a URI template string for constructing provenance-URIs. *)
val provenanceUriTemplate : Iri.t

(** If this Activity prov:wasAssociatedWith Agent :ag, then it can qualify the Association using prov:qualifiedAssociation [ a prov:Association;  prov:agent :ag; :foo :bar ]. *)
val qualifiedAssociation : Iri.t

(** If this Entity prov:wasAttributedTo Agent :ag, then it can qualify how it was influenced using prov:qualifiedAttribution [ a prov:Attribution;  prov:agent :ag; :foo :bar ]. *)
val qualifiedAttribution : Iri.t

(** If this Activity prov:wasInformedBy Activity :a, then it can qualify how it was influenced using prov:qualifiedCommunication [ a prov:Communication;  prov:activity :a; :foo :bar ]. *)
val qualifiedCommunication : Iri.t

(** If this Agent prov:actedOnBehalfOf Agent :ag, then it can qualify how with prov:qualifiedResponsibility [ a prov:Responsibility;  prov:agent :ag; :foo :bar ]. *)
val qualifiedDelegation : Iri.t

(** If this Entity prov:wasDerivedFrom Entity :e, then it can qualify how it was derived using prov:qualifiedDerivation [ a prov:Derivation;  prov:entity :e; :foo :bar ]. *)
val qualifiedDerivation : Iri.t

(** If this Activity prov:wasEndedBy Entity :e1, then it can qualify how it was ended using prov:qualifiedEnd [ a prov:End;  prov:entity :e1; :foo :bar ]. *)
val qualifiedEnd : Iri.t

(** This annotation property links a subproperty of prov:wasInfluencedBy with the subclass of prov:Influence and the qualifying property that are used to qualify it. 

Example annotation:

    prov:wasGeneratedBy prov:qualifiedForm prov:qualifiedGeneration, prov:Generation .

Then this unqualified assertion:

    :entity1 prov:wasGeneratedBy :activity1 .

can be qualified by adding:

   :entity1 prov:qualifiedGeneration :entity1Gen .
   :entity1Gen 
       a prov:Generation, prov:Influence;
       prov:activity :activity1;
       :customValue 1337 .

Note how the value of the unqualified influence (prov:wasGeneratedBy :activity1) is mirrored as the value of the prov:activity (or prov:entity, or prov:agent) property on the influence class. *)
val qualifiedForm : Iri.t

(** If this Activity prov:generated Entity :e, then it can qualify how it performed the Generation using prov:qualifiedGeneration [ a prov:Generation;  prov:entity :e; :foo :bar ]. *)
val qualifiedGeneration : Iri.t

(** Because prov:qualifiedInfluence is a broad relation, the more specific relations (qualifiedCommunication, qualifiedDelegation, qualifiedEnd, etc.) should be used when applicable. *)
val qualifiedInfluence : Iri.t

val qualifiedInsertion : Iri.t

(** If this Entity prov:wasInvalidatedBy Activity :a, then it can qualify how it was invalidated using prov:qualifiedInvalidation [ a prov:Invalidation;  prov:activity :a; :foo :bar ]. *)
val qualifiedInvalidation : Iri.t

(** If this Entity prov:hadPrimarySource Entity :e, then it can qualify how using prov:qualifiedPrimarySource [ a prov:PrimarySource; prov:entity :e; :foo :bar ]. *)
val qualifiedPrimarySource : Iri.t

(** If this Entity prov:wasQuotedFrom Entity :e, then it can qualify how using prov:qualifiedQuotation [ a prov:Quotation;  prov:entity :e; :foo :bar ]. *)
val qualifiedQuotation : Iri.t

val qualifiedRemoval : Iri.t

(** If this Entity prov:wasRevisionOf Entity :e, then it can qualify how it was revised using prov:qualifiedRevision [ a prov:Revision;  prov:entity :e; :foo :bar ]. *)
val qualifiedRevision : Iri.t

(** If this Activity prov:wasStartedBy Entity :e1, then it can qualify how it was started using prov:qualifiedStart [ a prov:Start;  prov:entity :e1; :foo :bar ]. *)
val qualifiedStart : Iri.t

(** If this Activity prov:used Entity :e, then it can qualify how it used it using prov:qualifiedUsage [ a prov:Usage; prov:entity :e; :foo :bar ]. *)
val qualifiedUsage : Iri.t

val removedKey : Iri.t

val sharesDefinitionWith : Iri.t

val specializationOf : Iri.t

(** The time at which an activity started. See also prov:endedAtTime. *)
val startedAtTime : Iri.t

val todo : Iri.t

(** Classes and properties used to qualify relationships are annotated with prov:unqualifiedForm to indicate the property used to assert an unqualified provenance relation. *)
val unqualifiedForm : Iri.t

(** A prov:Entity that was used by this prov:Activity. For example, :baking prov:used :spoon, :egg, :oven . *)
val used : Iri.t

val value : Iri.t

(** An prov:Agent that had some (unspecified) responsibility for the occurrence of this prov:Activity. *)
val wasAssociatedWith : Iri.t

(** Attribution is the ascribing of an entity to an agent. *)
val wasAttributedTo : Iri.t

(** The more specific subproperties of prov:wasDerivedFrom (i.e., prov:wasQuotedFrom, prov:wasRevisionOf, prov:hadPrimarySource) should be used when applicable. *)
val wasDerivedFrom : Iri.t

(** End is when an activity is deemed to have ended. An end may refer to an entity, known as trigger, that terminated the activity. *)
val wasEndedBy : Iri.t

val wasGeneratedBy : Iri.t

(** Because prov:wasInfluencedBy is a broad relation, its more specific subproperties (e.g. prov:wasInformedBy, prov:actedOnBehalfOf, prov:wasEndedBy, etc.) should be used when applicable. *)
val wasInfluencedBy : Iri.t

(** An activity a2 is dependent on or informed by another activity a1, by way of some unspecified entity that is generated by a1 and used by a2. *)
val wasInformedBy : Iri.t

val wasInvalidatedBy : Iri.t

(** An entity is derived from an original entity by copying, or 'quoting', some or all of it. *)
val wasQuotedFrom : Iri.t

(** A revision is a derivation that revises an entity into a revised version. *)
val wasRevisionOf : Iri.t

(** Start is when an activity is deemed to have started. A start may refer to an entity, known as trigger, that initiated the activity. *)
val wasStartedBy : Iri.t


module Open : sig
  val prov_c_Accept : Iri.t

  val prov_c_Activity : Iri.t

  (** It is not recommended that the type ActivityInfluence be asserted without also asserting one of its more specific subclasses. *)
  val prov_c_ActivityInfluence : Iri.t

  val prov_c_Agent : Iri.t

  (** It is not recommended that the type AgentInfluence be asserted without also asserting one of its more specific subclasses. *)
  val prov_c_AgentInfluence : Iri.t

  (** An instance of prov:Association provides additional descriptions about the binary prov:wasAssociatedWith relation from an prov:Activity to some prov:Agent that had some responsiblity for it. For example, :baking prov:wasAssociatedWith :baker; prov:qualifiedAssociation [ a prov:Association; prov:agent :baker; :foo :bar ]. *)
  val prov_c_Association : Iri.t

  (** An instance of prov:Attribution provides additional descriptions about the binary prov:wasAttributedTo relation from an prov:Entity to some prov:Agent that had some responsible for it. For example, :cake prov:wasAttributedTo :baker; prov:qualifiedAttribution [ a prov:Attribution; prov:entity :baker; :foo :bar ]. *)
  val prov_c_Attribution : Iri.t

  (** Note that there are kinds of bundles (e.g. handwritten letters, audio recordings, etc.) that are not expressed in PROV-O, but can be still be described by PROV-O. *)
  val prov_c_Bundle : Iri.t

  val prov_c_Collection : Iri.t

  (** An instance of prov:Communication provides additional descriptions about the binary prov:wasInformedBy relation from an informed prov:Activity to the prov:Activity that informed it. For example, :you_jumping_off_bridge prov:wasInformedBy :everyone_else_jumping_off_bridge; prov:qualifiedCommunication [ a prov:Communication; prov:activity :everyone_else_jumping_off_bridge; :foo :bar ]. *)
  val prov_c_Communication : Iri.t

  val prov_c_Contribute : Iri.t

  val prov_c_Contributor : Iri.t

  val prov_c_Copyright : Iri.t

  val prov_c_Create : Iri.t

  val prov_c_Creator : Iri.t

  (** An instance of prov:Delegation provides additional descriptions about the binary prov:actedOnBehalfOf relation from a performing prov:Agent to some prov:Agent for whom it was performed. For example, :mixing prov:wasAssociatedWith :toddler . :toddler prov:actedOnBehalfOf :mother; prov:qualifiedDelegation [ a prov:Delegation; prov:entity :mother; :foo :bar ]. *)
  val prov_c_Delegation : Iri.t

  (** The more specific forms of prov:Derivation (i.e., prov:Revision, prov:Quotation, prov:PrimarySource) should be asserted if they apply. *)
  val prov_c_Derivation : Iri.t

  (** This concept allows for the provenance of the dictionary, but also of its constituents to be expressed. Such a notion of dictionary corresponds to a wide variety of concrete data structures, such as a maps or associative arrays. *)
  val prov_c_Dictionary : Iri.t

  (** Type for a generic provenance query service. Mainly for use in RDF provenance query service descriptions, to facilitate discovery in linked data environments. *)
  val prov_c_DirectQueryService : Iri.t

  val prov_c_EmptyCollection : Iri.t

  val prov_c_EmptyDictionary : Iri.t

  (** An instance of prov:End provides additional descriptions about the binary prov:wasEndedBy relation from some ended prov:Activity to an prov:Entity that ended it. For example, :ball_game prov:wasEndedBy :buzzer; prov:qualifiedEnd [ a prov:End; prov:entity :buzzer; :foo :bar; prov:atTime '2012-03-09T08:05:08-05:00'^^xsd:dateTime ]. *)
  val prov_c_End : Iri.t

  val prov_c_Entity : Iri.t

  (** It is not recommended that the type EntityInfluence be asserted without also asserting one of its more specific subclasses. *)
  val prov_c_EntityInfluence : Iri.t

  (** An instance of prov:Generation provides additional descriptions about the binary prov:wasGeneratedBy relation from a generated prov:Entity to the prov:Activity that generated it. For example, :cake prov:wasGeneratedBy :baking; prov:qualifiedGeneration [ a prov:Generation; prov:activity :baking; :foo :bar ]. *)
  val prov_c_Generation : Iri.t

  (** Because prov:Influence is a broad relation, its most specific subclasses (e.g. prov:Communication, prov:Delegation, prov:End, prov:Revision, etc.) should be used when applicable. *)
  val prov_c_Influence : Iri.t

  val prov_c_Insertion : Iri.t

  (** An instantaneous event, or event for short, happens in the world and marks a change in the world, in its activities and in its entities. The term 'event' is commonly used in process algebra with a similar meaning. Events represent communications or interactions; they are assumed to be atomic and instantaneous. *)
  val prov_c_InstantaneousEvent : Iri.t

  (** An instance of prov:Invalidation provides additional descriptions about the binary prov:wasInvalidatedBy relation from an invalidated prov:Entity to the prov:Activity that invalidated it. For example, :uncracked_egg prov:wasInvalidatedBy :baking; prov:qualifiedInvalidation [ a prov:Invalidation; prov:activity :baking; :foo :bar ]. *)
  val prov_c_Invalidation : Iri.t

  val prov_c_KeyEntityPair : Iri.t

  val prov_c_Location : Iri.t

  val prov_c_Modify : Iri.t

  val prov_c_Organization : Iri.t

  val prov_c_Person : Iri.t

  (** There exist no prescriptive requirement on the nature of plans, their representation, the actions or steps they consist of, or their intended goals. Since plans may evolve over time, it may become necessary to track their provenance, so plans themselves are entities. Representing the plan explicitly in the provenance can be useful for various tasks: for example, to validate the execution as represented in the provenance record, to manage expectation failures, or to provide explanations. *)
  val prov_c_Plan : Iri.t

  (** An instance of prov:PrimarySource provides additional descriptions about the binary prov:hadPrimarySource relation from some secondary prov:Entity to an earlier, primary prov:Entity. For example, :blog prov:hadPrimarySource :newsArticle; prov:qualifiedPrimarySource [ a prov:PrimarySource; prov:entity :newsArticle; :foo :bar ] . *)
  val prov_c_PrimarySource : Iri.t

  val prov_c_Publish : Iri.t

  val prov_c_Publisher : Iri.t

  (** An instance of prov:Quotation provides additional descriptions about the binary prov:wasQuotedFrom relation from some taken prov:Entity from an earlier, larger prov:Entity. For example, :here_is_looking_at_you_kid prov:wasQuotedFrom :casablanca_script; prov:qualifiedQuotation [ a prov:Quotation; prov:entity :casablanca_script; :foo :bar ]. *)
  val prov_c_Quotation : Iri.t

  val prov_c_Removal : Iri.t

  val prov_c_Replace : Iri.t

  (** An instance of prov:Revision provides additional descriptions about the binary prov:wasRevisionOf relation from some newer prov:Entity to an earlier prov:Entity. For example, :draft_2 prov:wasRevisionOf :draft_1; prov:qualifiedRevision [ a prov:Revision; prov:entity :draft_1; :foo :bar ]. *)
  val prov_c_Revision : Iri.t

  val prov_c_RightsAssignment : Iri.t

  val prov_c_RightsHolder : Iri.t

  val prov_c_Role : Iri.t

  (** Type for a generic provenance query service. Mainly for use in RDF provenance query service descriptions, to facilitate discovery in linked data environments. *)
  val prov_c_ServiceDescription : Iri.t

  val prov_c_SoftwareAgent : Iri.t

  (** An instance of prov:Start provides additional descriptions about the binary prov:wasStartedBy relation from some started prov:Activity to an prov:Entity that started it. For example, :foot_race prov:wasStartedBy :bang; prov:qualifiedStart [ a prov:Start; prov:entity :bang; :foo :bar; prov:atTime '2012-03-09T08:05:08-05:00'^^xsd:dateTime ] . *)
  val prov_c_Start : Iri.t

  val prov_c_Submit : Iri.t

  (** An instance of prov:Usage provides additional descriptions about the binary prov:used relation from some prov:Activity to an prov:Entity that it used. For example, :keynote prov:used :podium; prov:qualifiedUsage [ a prov:Usage; prov:entity :podium; :foo :bar ]. *)
  val prov_c_Usage : Iri.t

  (** An object property to express the accountability of an agent towards another agent. The subordinate agent acted on behalf of the responsible agent in an actual activity.  *)
  val prov_actedOnBehalfOf : Iri.t

  val prov_activity : Iri.t

  val prov_agent : Iri.t

  val prov_alternateOf : Iri.t

  val prov_aq : Iri.t

  (** prov:asInBundle is used to specify which bundle the general entity of a prov:mentionOf property is described.

When :x prov:mentionOf :y and :y is described in Bundle :b, the triple :x prov:asInBundle :b is also asserted to cite the Bundle in which :y was described. *)
  val prov_asInBundle : Iri.t

  (** The Location of any resource. *)
  val prov_atLocation : Iri.t

  (** The time at which an InstantaneousEvent occurred, in the form of xsd:dateTime. *)
  val prov_atTime : Iri.t

  (** Classify prov-o terms into three categories, including 'starting-point', 'qualifed', and 'extended'. This classification is used by the prov-o html document to gently introduce prov-o terms to its users.  *)
  val prov_category : Iri.t

  (** Classify prov-o terms into six components according to prov-dm, including 'agents-responsibility', 'alternate', 'annotations', 'collections', 'derivations', and 'entities-activities'. This classification is used so that readers of prov-o specification can find its correspondence with the prov-dm specification. *)
  val prov_component : Iri.t

  (** A reference to the principal section of the PROV-CONSTRAINTS document that describes this concept. *)
  val prov_constraints : Iri.t

  (** A definition quoted from PROV-DM or PROV-CONSTRAINTS that describes the concept expressed with this OWL term. *)
  val prov_definition : Iri.t

  val prov_derivedByInsertionFrom : Iri.t

  val prov_derivedByRemovalFrom : Iri.t

  (** relates a generic provenance query service resource (type prov:ServiceDescription) to a specific query service description (e.g. a prov:DirectQueryService or a sd:Service). *)
  val prov_describesService : Iri.t

  val prov_dictionary : Iri.t

  (** A reference to the principal section of the PROV-DM document that describes this concept. *)
  val prov_dm : Iri.t

  (** A note by the OWL development team about how this term expresses the PROV-DM concept, or how it should be used in context of semantic web or linked data. *)
  val prov_editorialNote : Iri.t

  (** When the prov-o term does not have a definition drawn from prov-dm, and the prov-o editor provides one. *)
  val prov_editorsDefinition : Iri.t

  (** The time at which an activity ended. See also prov:startedAtTime. *)
  val prov_endedAtTime : Iri.t

  val prov_entity : Iri.t

  val prov_generated : Iri.t

  (** The time at which an entity was completely created and is available for use. *)
  val prov_generatedAtTime : Iri.t

  (** The _optional_ Activity of an Influence, which used, generated, invalidated, or was the responsibility of some Entity. This property is _not_ used by ActivityInfluence (use prov:activity instead). *)
  val prov_hadActivity : Iri.t

  val prov_hadDictionaryMember : Iri.t

  (** The _optional_ Generation involved in an Entity's Derivation. *)
  val prov_hadGeneration : Iri.t

  val prov_hadMember : Iri.t

  (** The _optional_ Plan adopted by an Agent in Association with some Activity. Plan specifications are out of the scope of this specification. *)
  val prov_hadPlan : Iri.t

  val prov_hadPrimarySource : Iri.t

  (** The _optional_ Role that an Entity assumed in the context of an Activity. For example, :baking prov:used :spoon; prov:qualified [ a prov:Usage; prov:entity :spoon; prov:hadRole roles:mixing_implement ]. *)
  val prov_hadRole : Iri.t

  (** The _optional_ Usage involved in an Entity's Derivation. *)
  val prov_hadUsage : Iri.t

  (** Indicates anchor URI for a potentially dynamic resource instance. *)
  val prov_has_anchor : Iri.t

  (** Indicates a provenance-URI for a resource; the resource identified by this property presents a provenance record about its subject or anchor resource. *)
  val prov_has_provenance : Iri.t

  (** Indicates a provenance query service that can access provenance related to its subject or anchor resource. *)
  val prov_has_query_service : Iri.t

  val prov_influenced : Iri.t

  (** Subproperties of prov:influencer are used to cite the object of an unqualified PROV-O triple whose predicate is a subproperty of prov:wasInfluencedBy (e.g. prov:used, prov:wasGeneratedBy). prov:influencer is used much like rdf:object is used. *)
  val prov_influencer : Iri.t

  val prov_insertedKeyEntityPair : Iri.t

  val prov_invalidated : Iri.t

  (** The time at which an entity was invalidated (i.e., no longer usable). *)
  val prov_invalidatedAtTime : Iri.t

  (** PROV-O does not define all property inverses. The directionalities defined in PROV-O should be given preference over those not defined. However, if users wish to name the inverse of a PROV-O property, the local name given by prov:inverse should be used. *)
  val prov_inverse : Iri.t

  (** prov:mentionOf is used to specialize an entity as described in another bundle. It is to be used in conjuction with prov:asInBundle.

prov:asInBundle is used to cite the Bundle in which the generalization was mentioned. *)
  val prov_mentionOf : Iri.t

  (** A reference to the principal section of the PROV-M document that describes this concept. *)
  val prov_n : Iri.t

  (** The position that this OWL term should be listed within documentation. The scope of the documentation (e.g., among all terms, among terms within a prov:category, among properties applying to a particular class, etc.) is unspecified. *)
  val prov_order : Iri.t

  val prov_pairEntity : Iri.t

  val prov_pairKey : Iri.t

  (** Relates a resource to a provenance pingback service that may receive additional provenance links about the resource. *)
  val prov_pingback : Iri.t

  (** Relates a provenance service to a URI template string for constructing provenance-URIs. *)
  val prov_provenanceUriTemplate : Iri.t

  (** If this Activity prov:wasAssociatedWith Agent :ag, then it can qualify the Association using prov:qualifiedAssociation [ a prov:Association;  prov:agent :ag; :foo :bar ]. *)
  val prov_qualifiedAssociation : Iri.t

  (** If this Entity prov:wasAttributedTo Agent :ag, then it can qualify how it was influenced using prov:qualifiedAttribution [ a prov:Attribution;  prov:agent :ag; :foo :bar ]. *)
  val prov_qualifiedAttribution : Iri.t

  (** If this Activity prov:wasInformedBy Activity :a, then it can qualify how it was influenced using prov:qualifiedCommunication [ a prov:Communication;  prov:activity :a; :foo :bar ]. *)
  val prov_qualifiedCommunication : Iri.t

  (** If this Agent prov:actedOnBehalfOf Agent :ag, then it can qualify how with prov:qualifiedResponsibility [ a prov:Responsibility;  prov:agent :ag; :foo :bar ]. *)
  val prov_qualifiedDelegation : Iri.t

  (** If this Entity prov:wasDerivedFrom Entity :e, then it can qualify how it was derived using prov:qualifiedDerivation [ a prov:Derivation;  prov:entity :e; :foo :bar ]. *)
  val prov_qualifiedDerivation : Iri.t

  (** If this Activity prov:wasEndedBy Entity :e1, then it can qualify how it was ended using prov:qualifiedEnd [ a prov:End;  prov:entity :e1; :foo :bar ]. *)
  val prov_qualifiedEnd : Iri.t

  (** This annotation property links a subproperty of prov:wasInfluencedBy with the subclass of prov:Influence and the qualifying property that are used to qualify it. 

Example annotation:

    prov:wasGeneratedBy prov:qualifiedForm prov:qualifiedGeneration, prov:Generation .

Then this unqualified assertion:

    :entity1 prov:wasGeneratedBy :activity1 .

can be qualified by adding:

   :entity1 prov:qualifiedGeneration :entity1Gen .
   :entity1Gen 
       a prov:Generation, prov:Influence;
       prov:activity :activity1;
       :customValue 1337 .

Note how the value of the unqualified influence (prov:wasGeneratedBy :activity1) is mirrored as the value of the prov:activity (or prov:entity, or prov:agent) property on the influence class. *)
  val prov_qualifiedForm : Iri.t

  (** If this Activity prov:generated Entity :e, then it can qualify how it performed the Generation using prov:qualifiedGeneration [ a prov:Generation;  prov:entity :e; :foo :bar ]. *)
  val prov_qualifiedGeneration : Iri.t

  (** Because prov:qualifiedInfluence is a broad relation, the more specific relations (qualifiedCommunication, qualifiedDelegation, qualifiedEnd, etc.) should be used when applicable. *)
  val prov_qualifiedInfluence : Iri.t

  val prov_qualifiedInsertion : Iri.t

  (** If this Entity prov:wasInvalidatedBy Activity :a, then it can qualify how it was invalidated using prov:qualifiedInvalidation [ a prov:Invalidation;  prov:activity :a; :foo :bar ]. *)
  val prov_qualifiedInvalidation : Iri.t

  (** If this Entity prov:hadPrimarySource Entity :e, then it can qualify how using prov:qualifiedPrimarySource [ a prov:PrimarySource; prov:entity :e; :foo :bar ]. *)
  val prov_qualifiedPrimarySource : Iri.t

  (** If this Entity prov:wasQuotedFrom Entity :e, then it can qualify how using prov:qualifiedQuotation [ a prov:Quotation;  prov:entity :e; :foo :bar ]. *)
  val prov_qualifiedQuotation : Iri.t

  val prov_qualifiedRemoval : Iri.t

  (** If this Entity prov:wasRevisionOf Entity :e, then it can qualify how it was revised using prov:qualifiedRevision [ a prov:Revision;  prov:entity :e; :foo :bar ]. *)
  val prov_qualifiedRevision : Iri.t

  (** If this Activity prov:wasStartedBy Entity :e1, then it can qualify how it was started using prov:qualifiedStart [ a prov:Start;  prov:entity :e1; :foo :bar ]. *)
  val prov_qualifiedStart : Iri.t

  (** If this Activity prov:used Entity :e, then it can qualify how it used it using prov:qualifiedUsage [ a prov:Usage; prov:entity :e; :foo :bar ]. *)
  val prov_qualifiedUsage : Iri.t

  val prov_removedKey : Iri.t

  val prov_sharesDefinitionWith : Iri.t

  val prov_specializationOf : Iri.t

  (** The time at which an activity started. See also prov:endedAtTime. *)
  val prov_startedAtTime : Iri.t

  val prov_todo : Iri.t

  (** Classes and properties used to qualify relationships are annotated with prov:unqualifiedForm to indicate the property used to assert an unqualified provenance relation. *)
  val prov_unqualifiedForm : Iri.t

  (** A prov:Entity that was used by this prov:Activity. For example, :baking prov:used :spoon, :egg, :oven . *)
  val prov_used : Iri.t

  val prov_value : Iri.t

  (** An prov:Agent that had some (unspecified) responsibility for the occurrence of this prov:Activity. *)
  val prov_wasAssociatedWith : Iri.t

  (** Attribution is the ascribing of an entity to an agent. *)
  val prov_wasAttributedTo : Iri.t

  (** The more specific subproperties of prov:wasDerivedFrom (i.e., prov:wasQuotedFrom, prov:wasRevisionOf, prov:hadPrimarySource) should be used when applicable. *)
  val prov_wasDerivedFrom : Iri.t

  (** End is when an activity is deemed to have ended. An end may refer to an entity, known as trigger, that terminated the activity. *)
  val prov_wasEndedBy : Iri.t

  val prov_wasGeneratedBy : Iri.t

  (** Because prov:wasInfluencedBy is a broad relation, its more specific subproperties (e.g. prov:wasInformedBy, prov:actedOnBehalfOf, prov:wasEndedBy, etc.) should be used when applicable. *)
  val prov_wasInfluencedBy : Iri.t

  (** An activity a2 is dependent on or informed by another activity a1, by way of some unspecified entity that is generated by a1 and used by a2. *)
  val prov_wasInformedBy : Iri.t

  val prov_wasInvalidatedBy : Iri.t

  (** An entity is derived from an original entity by copying, or 'quoting', some or all of it. *)
  val prov_wasQuotedFrom : Iri.t

  (** A revision is a derivation that revises an entity into a revised version. *)
  val prov_wasRevisionOf : Iri.t

  (** Start is when an activity is deemed to have started. A start may refer to an entity, known as trigger, that initiated the activity. *)
  val prov_wasStartedBy : Iri.t

end

class from : ?sub: Rdf_term.term -> Rdf_graph.graph ->
  object
    method actedOnBehalfOf : Rdf_term.term list
    method actedOnBehalfOf_opt : Rdf_term.term option
    method actedOnBehalfOf_iris : Iri.t list
    method actedOnBehalfOf_opt_iri : Iri.t option
    method activity : Rdf_term.term list
    method activity_opt : Rdf_term.term option
    method activity_iris : Iri.t list
    method activity_opt_iri : Iri.t option
    method agent : Rdf_term.term list
    method agent_opt : Rdf_term.term option
    method agent_iris : Iri.t list
    method agent_opt_iri : Iri.t option
    method alternateOf : Rdf_term.term list
    method alternateOf_opt : Rdf_term.term option
    method alternateOf_iris : Iri.t list
    method alternateOf_opt_iri : Iri.t option
    method aq : Rdf_term.term list
    method aq_opt : Rdf_term.term option
    method aq_iris : Iri.t list
    method aq_opt_iri : Iri.t option
    method asInBundle : Rdf_term.term list
    method asInBundle_opt : Rdf_term.term option
    method asInBundle_iris : Iri.t list
    method asInBundle_opt_iri : Iri.t option
    method atLocation : Rdf_term.term list
    method atLocation_opt : Rdf_term.term option
    method atLocation_iris : Iri.t list
    method atLocation_opt_iri : Iri.t option
    method atTime : Rdf_term.literal list
    method atTime_opt : Rdf_term.literal option
    method category : Rdf_term.term list
    method category_opt : Rdf_term.term option
    method category_iris : Iri.t list
    method category_opt_iri : Iri.t option
    method component : Rdf_term.term list
    method component_opt : Rdf_term.term option
    method component_iris : Iri.t list
    method component_opt_iri : Iri.t option
    method constraints : Rdf_term.term list
    method constraints_opt : Rdf_term.term option
    method constraints_iris : Iri.t list
    method constraints_opt_iri : Iri.t option
    method definition : Rdf_term.term list
    method definition_opt : Rdf_term.term option
    method definition_iris : Iri.t list
    method definition_opt_iri : Iri.t option
    method derivedByInsertionFrom : Rdf_term.term list
    method derivedByInsertionFrom_opt : Rdf_term.term option
    method derivedByInsertionFrom_iris : Iri.t list
    method derivedByInsertionFrom_opt_iri : Iri.t option
    method derivedByRemovalFrom : Rdf_term.term list
    method derivedByRemovalFrom_opt : Rdf_term.term option
    method derivedByRemovalFrom_iris : Iri.t list
    method derivedByRemovalFrom_opt_iri : Iri.t option
    method describesService : Rdf_term.term list
    method describesService_opt : Rdf_term.term option
    method describesService_iris : Iri.t list
    method describesService_opt_iri : Iri.t option
    method dictionary : Rdf_term.term list
    method dictionary_opt : Rdf_term.term option
    method dictionary_iris : Iri.t list
    method dictionary_opt_iri : Iri.t option
    method dm : Rdf_term.term list
    method dm_opt : Rdf_term.term option
    method dm_iris : Iri.t list
    method dm_opt_iri : Iri.t option
    method editorialNote : Rdf_term.term list
    method editorialNote_opt : Rdf_term.term option
    method editorialNote_iris : Iri.t list
    method editorialNote_opt_iri : Iri.t option
    method editorsDefinition : Rdf_term.term list
    method editorsDefinition_opt : Rdf_term.term option
    method editorsDefinition_iris : Iri.t list
    method editorsDefinition_opt_iri : Iri.t option
    method endedAtTime : Rdf_term.literal list
    method endedAtTime_opt : Rdf_term.literal option
    method entity : Rdf_term.term list
    method entity_opt : Rdf_term.term option
    method entity_iris : Iri.t list
    method entity_opt_iri : Iri.t option
    method generated : Rdf_term.term list
    method generated_opt : Rdf_term.term option
    method generated_iris : Iri.t list
    method generated_opt_iri : Iri.t option
    method generatedAtTime : Rdf_term.literal list
    method generatedAtTime_opt : Rdf_term.literal option
    method hadActivity : Rdf_term.term list
    method hadActivity_opt : Rdf_term.term option
    method hadActivity_iris : Iri.t list
    method hadActivity_opt_iri : Iri.t option
    method hadDictionaryMember : Rdf_term.term list
    method hadDictionaryMember_opt : Rdf_term.term option
    method hadDictionaryMember_iris : Iri.t list
    method hadDictionaryMember_opt_iri : Iri.t option
    method hadGeneration : Rdf_term.term list
    method hadGeneration_opt : Rdf_term.term option
    method hadGeneration_iris : Iri.t list
    method hadGeneration_opt_iri : Iri.t option
    method hadMember : Rdf_term.term list
    method hadMember_opt : Rdf_term.term option
    method hadMember_iris : Iri.t list
    method hadMember_opt_iri : Iri.t option
    method hadPlan : Rdf_term.term list
    method hadPlan_opt : Rdf_term.term option
    method hadPlan_iris : Iri.t list
    method hadPlan_opt_iri : Iri.t option
    method hadPrimarySource : Rdf_term.term list
    method hadPrimarySource_opt : Rdf_term.term option
    method hadPrimarySource_iris : Iri.t list
    method hadPrimarySource_opt_iri : Iri.t option
    method hadRole : Rdf_term.term list
    method hadRole_opt : Rdf_term.term option
    method hadRole_iris : Iri.t list
    method hadRole_opt_iri : Iri.t option
    method hadUsage : Rdf_term.term list
    method hadUsage_opt : Rdf_term.term option
    method hadUsage_iris : Iri.t list
    method hadUsage_opt_iri : Iri.t option
    method has_anchor : Rdf_term.term list
    method has_anchor_opt : Rdf_term.term option
    method has_anchor_iris : Iri.t list
    method has_anchor_opt_iri : Iri.t option
    method has_provenance : Rdf_term.term list
    method has_provenance_opt : Rdf_term.term option
    method has_provenance_iris : Iri.t list
    method has_provenance_opt_iri : Iri.t option
    method has_query_service : Rdf_term.term list
    method has_query_service_opt : Rdf_term.term option
    method has_query_service_iris : Iri.t list
    method has_query_service_opt_iri : Iri.t option
    method influenced : Rdf_term.term list
    method influenced_opt : Rdf_term.term option
    method influenced_iris : Iri.t list
    method influenced_opt_iri : Iri.t option
    method influencer : Rdf_term.term list
    method influencer_opt : Rdf_term.term option
    method influencer_iris : Iri.t list
    method influencer_opt_iri : Iri.t option
    method insertedKeyEntityPair : Rdf_term.term list
    method insertedKeyEntityPair_opt : Rdf_term.term option
    method insertedKeyEntityPair_iris : Iri.t list
    method insertedKeyEntityPair_opt_iri : Iri.t option
    method invalidated : Rdf_term.term list
    method invalidated_opt : Rdf_term.term option
    method invalidated_iris : Iri.t list
    method invalidated_opt_iri : Iri.t option
    method invalidatedAtTime : Rdf_term.literal list
    method invalidatedAtTime_opt : Rdf_term.literal option
    method inverse : Rdf_term.term list
    method inverse_opt : Rdf_term.term option
    method inverse_iris : Iri.t list
    method inverse_opt_iri : Iri.t option
    method mentionOf : Rdf_term.term list
    method mentionOf_opt : Rdf_term.term option
    method mentionOf_iris : Iri.t list
    method mentionOf_opt_iri : Iri.t option
    method n : Rdf_term.term list
    method n_opt : Rdf_term.term option
    method n_iris : Iri.t list
    method n_opt_iri : Iri.t option
    method order : Rdf_term.term list
    method order_opt : Rdf_term.term option
    method order_iris : Iri.t list
    method order_opt_iri : Iri.t option
    method pairEntity : Rdf_term.term list
    method pairEntity_opt : Rdf_term.term option
    method pairEntity_iris : Iri.t list
    method pairEntity_opt_iri : Iri.t option
    method pairKey : Rdf_term.literal list
    method pairKey_opt : Rdf_term.literal option
    method pingback : Rdf_term.term list
    method pingback_opt : Rdf_term.term option
    method pingback_iris : Iri.t list
    method pingback_opt_iri : Iri.t option
    method provenanceUriTemplate : Rdf_term.literal list
    method provenanceUriTemplate_opt : Rdf_term.literal option
    method qualifiedAssociation : Rdf_term.term list
    method qualifiedAssociation_opt : Rdf_term.term option
    method qualifiedAssociation_iris : Iri.t list
    method qualifiedAssociation_opt_iri : Iri.t option
    method qualifiedAttribution : Rdf_term.term list
    method qualifiedAttribution_opt : Rdf_term.term option
    method qualifiedAttribution_iris : Iri.t list
    method qualifiedAttribution_opt_iri : Iri.t option
    method qualifiedCommunication : Rdf_term.term list
    method qualifiedCommunication_opt : Rdf_term.term option
    method qualifiedCommunication_iris : Iri.t list
    method qualifiedCommunication_opt_iri : Iri.t option
    method qualifiedDelegation : Rdf_term.term list
    method qualifiedDelegation_opt : Rdf_term.term option
    method qualifiedDelegation_iris : Iri.t list
    method qualifiedDelegation_opt_iri : Iri.t option
    method qualifiedDerivation : Rdf_term.term list
    method qualifiedDerivation_opt : Rdf_term.term option
    method qualifiedDerivation_iris : Iri.t list
    method qualifiedDerivation_opt_iri : Iri.t option
    method qualifiedEnd : Rdf_term.term list
    method qualifiedEnd_opt : Rdf_term.term option
    method qualifiedEnd_iris : Iri.t list
    method qualifiedEnd_opt_iri : Iri.t option
    method qualifiedForm : Rdf_term.term list
    method qualifiedForm_opt : Rdf_term.term option
    method qualifiedForm_iris : Iri.t list
    method qualifiedForm_opt_iri : Iri.t option
    method qualifiedGeneration : Rdf_term.term list
    method qualifiedGeneration_opt : Rdf_term.term option
    method qualifiedGeneration_iris : Iri.t list
    method qualifiedGeneration_opt_iri : Iri.t option
    method qualifiedInfluence : Rdf_term.term list
    method qualifiedInfluence_opt : Rdf_term.term option
    method qualifiedInfluence_iris : Iri.t list
    method qualifiedInfluence_opt_iri : Iri.t option
    method qualifiedInsertion : Rdf_term.term list
    method qualifiedInsertion_opt : Rdf_term.term option
    method qualifiedInsertion_iris : Iri.t list
    method qualifiedInsertion_opt_iri : Iri.t option
    method qualifiedInvalidation : Rdf_term.term list
    method qualifiedInvalidation_opt : Rdf_term.term option
    method qualifiedInvalidation_iris : Iri.t list
    method qualifiedInvalidation_opt_iri : Iri.t option
    method qualifiedPrimarySource : Rdf_term.term list
    method qualifiedPrimarySource_opt : Rdf_term.term option
    method qualifiedPrimarySource_iris : Iri.t list
    method qualifiedPrimarySource_opt_iri : Iri.t option
    method qualifiedQuotation : Rdf_term.term list
    method qualifiedQuotation_opt : Rdf_term.term option
    method qualifiedQuotation_iris : Iri.t list
    method qualifiedQuotation_opt_iri : Iri.t option
    method qualifiedRemoval : Rdf_term.term list
    method qualifiedRemoval_opt : Rdf_term.term option
    method qualifiedRemoval_iris : Iri.t list
    method qualifiedRemoval_opt_iri : Iri.t option
    method qualifiedRevision : Rdf_term.term list
    method qualifiedRevision_opt : Rdf_term.term option
    method qualifiedRevision_iris : Iri.t list
    method qualifiedRevision_opt_iri : Iri.t option
    method qualifiedStart : Rdf_term.term list
    method qualifiedStart_opt : Rdf_term.term option
    method qualifiedStart_iris : Iri.t list
    method qualifiedStart_opt_iri : Iri.t option
    method qualifiedUsage : Rdf_term.term list
    method qualifiedUsage_opt : Rdf_term.term option
    method qualifiedUsage_iris : Iri.t list
    method qualifiedUsage_opt_iri : Iri.t option
    method removedKey : Rdf_term.literal list
    method removedKey_opt : Rdf_term.literal option
    method sharesDefinitionWith : Rdf_term.term list
    method sharesDefinitionWith_opt : Rdf_term.term option
    method sharesDefinitionWith_iris : Iri.t list
    method sharesDefinitionWith_opt_iri : Iri.t option
    method specializationOf : Rdf_term.term list
    method specializationOf_opt : Rdf_term.term option
    method specializationOf_iris : Iri.t list
    method specializationOf_opt_iri : Iri.t option
    method startedAtTime : Rdf_term.literal list
    method startedAtTime_opt : Rdf_term.literal option
    method todo : Rdf_term.term list
    method todo_opt : Rdf_term.term option
    method todo_iris : Iri.t list
    method todo_opt_iri : Iri.t option
    method unqualifiedForm : Rdf_term.term list
    method unqualifiedForm_opt : Rdf_term.term option
    method unqualifiedForm_iris : Iri.t list
    method unqualifiedForm_opt_iri : Iri.t option
    method used : Rdf_term.term list
    method used_opt : Rdf_term.term option
    method used_iris : Iri.t list
    method used_opt_iri : Iri.t option
    method value : Rdf_term.literal list
    method value_opt : Rdf_term.literal option
    method wasAssociatedWith : Rdf_term.term list
    method wasAssociatedWith_opt : Rdf_term.term option
    method wasAssociatedWith_iris : Iri.t list
    method wasAssociatedWith_opt_iri : Iri.t option
    method wasAttributedTo : Rdf_term.term list
    method wasAttributedTo_opt : Rdf_term.term option
    method wasAttributedTo_iris : Iri.t list
    method wasAttributedTo_opt_iri : Iri.t option
    method wasDerivedFrom : Rdf_term.term list
    method wasDerivedFrom_opt : Rdf_term.term option
    method wasDerivedFrom_iris : Iri.t list
    method wasDerivedFrom_opt_iri : Iri.t option
    method wasEndedBy : Rdf_term.term list
    method wasEndedBy_opt : Rdf_term.term option
    method wasEndedBy_iris : Iri.t list
    method wasEndedBy_opt_iri : Iri.t option
    method wasGeneratedBy : Rdf_term.term list
    method wasGeneratedBy_opt : Rdf_term.term option
    method wasGeneratedBy_iris : Iri.t list
    method wasGeneratedBy_opt_iri : Iri.t option
    method wasInfluencedBy : Rdf_term.term list
    method wasInfluencedBy_opt : Rdf_term.term option
    method wasInfluencedBy_iris : Iri.t list
    method wasInfluencedBy_opt_iri : Iri.t option
    method wasInformedBy : Rdf_term.term list
    method wasInformedBy_opt : Rdf_term.term option
    method wasInformedBy_iris : Iri.t list
    method wasInformedBy_opt_iri : Iri.t option
    method wasInvalidatedBy : Rdf_term.term list
    method wasInvalidatedBy_opt : Rdf_term.term option
    method wasInvalidatedBy_iris : Iri.t list
    method wasInvalidatedBy_opt_iri : Iri.t option
    method wasQuotedFrom : Rdf_term.term list
    method wasQuotedFrom_opt : Rdf_term.term option
    method wasQuotedFrom_iris : Iri.t list
    method wasQuotedFrom_opt_iri : Iri.t option
    method wasRevisionOf : Rdf_term.term list
    method wasRevisionOf_opt : Rdf_term.term option
    method wasRevisionOf_iris : Iri.t list
    method wasRevisionOf_opt_iri : Iri.t option
    method wasStartedBy : Rdf_term.term list
    method wasStartedBy_opt : Rdf_term.term option
    method wasStartedBy_iris : Iri.t list
    method wasStartedBy_opt_iri : Iri.t option
  end

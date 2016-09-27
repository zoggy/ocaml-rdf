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

class from : ?sub: Iri.t -> Rdf_graph.graph ->
  object
    method actedOnBehalfOf : Iri.t list
    method activity : Iri.t list
    method agent : Iri.t list
    method alternateOf : Iri.t list
    method aq : Iri.t list
    method asInBundle : Iri.t list
    method atLocation : Iri.t list
    method atTime : Rdf_term.literal list
    method category : Iri.t list
    method component : Iri.t list
    method constraints : Iri.t list
    method definition : Iri.t list
    method derivedByInsertionFrom : Iri.t list
    method derivedByRemovalFrom : Iri.t list
    method describesService : Iri.t list
    method dictionary : Iri.t list
    method dm : Iri.t list
    method editorialNote : Iri.t list
    method editorsDefinition : Iri.t list
    method endedAtTime : Rdf_term.literal list
    method entity : Iri.t list
    method generated : Iri.t list
    method generatedAtTime : Rdf_term.literal list
    method hadActivity : Iri.t list
    method hadDictionaryMember : Iri.t list
    method hadGeneration : Iri.t list
    method hadMember : Iri.t list
    method hadPlan : Iri.t list
    method hadPrimarySource : Iri.t list
    method hadRole : Iri.t list
    method hadUsage : Iri.t list
    method has_anchor : Iri.t list
    method has_provenance : Iri.t list
    method has_query_service : Iri.t list
    method influenced : Iri.t list
    method influencer : Iri.t list
    method insertedKeyEntityPair : Iri.t list
    method invalidated : Iri.t list
    method invalidatedAtTime : Rdf_term.literal list
    method inverse : Iri.t list
    method mentionOf : Iri.t list
    method n : Iri.t list
    method order : Iri.t list
    method pairEntity : Iri.t list
    method pairKey : Rdf_term.literal list
    method pingback : Iri.t list
    method provenanceUriTemplate : Rdf_term.literal list
    method qualifiedAssociation : Iri.t list
    method qualifiedAttribution : Iri.t list
    method qualifiedCommunication : Iri.t list
    method qualifiedDelegation : Iri.t list
    method qualifiedDerivation : Iri.t list
    method qualifiedEnd : Iri.t list
    method qualifiedForm : Iri.t list
    method qualifiedGeneration : Iri.t list
    method qualifiedInfluence : Iri.t list
    method qualifiedInsertion : Iri.t list
    method qualifiedInvalidation : Iri.t list
    method qualifiedPrimarySource : Iri.t list
    method qualifiedQuotation : Iri.t list
    method qualifiedRemoval : Iri.t list
    method qualifiedRevision : Iri.t list
    method qualifiedStart : Iri.t list
    method qualifiedUsage : Iri.t list
    method removedKey : Rdf_term.literal list
    method sharesDefinitionWith : Iri.t list
    method specializationOf : Iri.t list
    method startedAtTime : Rdf_term.literal list
    method todo : Iri.t list
    method unqualifiedForm : Iri.t list
    method used : Iri.t list
    method value : Rdf_term.literal list
    method wasAssociatedWith : Iri.t list
    method wasAttributedTo : Iri.t list
    method wasDerivedFrom : Iri.t list
    method wasEndedBy : Iri.t list
    method wasGeneratedBy : Iri.t list
    method wasInfluencedBy : Iri.t list
    method wasInformedBy : Iri.t list
    method wasInvalidatedBy : Iri.t list
    method wasQuotedFrom : Iri.t list
    method wasRevisionOf : Iri.t list
    method wasStartedBy : Iri.t list
  end

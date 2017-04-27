(** Elements of [http://purl.org/dc/terms/] *)

(** [http://purl.org/dc/terms/] *)
val dc : Iri.t
val dc_ : string -> Iri.t

(** A resource that acts or has the power to act. *)
val c_Agent : Iri.t

(** A group of agents. *)
val c_AgentClass : Iri.t

(** A book, article, or other documentary resource. *)
val c_BibliographicResource : Iri.t

(** The set of regions in space defined by their geographic coordinates according to the DCMI Box Encoding Scheme. *)
val dt_Box : Iri.t

(** A digital resource format. *)
val c_FileFormat : Iri.t

(** A rate at which something recurs. *)
val c_Frequency : Iri.t

(** The set of codes listed in ISO 3166-1 for the representation of names of countries. *)
val dt_ISO3166 : Iri.t

(** The three-letter alphabetic codes listed in ISO639-2 for the representation of names of languages. *)
val dt_ISO639_2 : Iri.t

(** The set of three-letter codes listed in ISO 639-3 for the representation of names of languages. *)
val dt_ISO639_3 : Iri.t

(** The extent or range of judicial, law enforcement, or other authority. *)
val c_Jurisdiction : Iri.t

(** A legal document giving official permission to do something with a Resource. *)
val c_LicenseDocument : Iri.t

(** A system of signs, symbols, sounds, gestures, or rules used in communication. *)
val c_LinguisticSystem : Iri.t

(** A spatial region or named place. *)
val c_Location : Iri.t

(** A location, period of time, or jurisdiction. *)
val c_LocationPeriodOrJurisdiction : Iri.t

(** A file format or physical medium. *)
val c_MediaType : Iri.t

(** A media type or extent. *)
val c_MediaTypeOrExtent : Iri.t

(** A method by which resources are added to a collection. *)
val c_MethodOfAccrual : Iri.t

(** A process that is used to engender knowledge, attitudes, and skills. *)
val c_MethodOfInstruction : Iri.t

(** The set of time intervals defined by their limits according to the DCMI Period Encoding Scheme. *)
val dt_Period : Iri.t

(** An interval of time that is named or defined by its start and end dates. *)
val c_PeriodOfTime : Iri.t

(** A physical material or carrier. *)
val c_PhysicalMedium : Iri.t

(** A material thing. *)
val c_PhysicalResource : Iri.t

(** The set of points in space defined by their geographic coordinates according to the DCMI Point Encoding Scheme. *)
val dt_Point : Iri.t

(** A plan or course of action by an authority, intended to influence and determine decisions, actions, and other matters. *)
val c_Policy : Iri.t

(** A statement of any changes in ownership and custody of a resource since its creation that are significant for its authenticity, integrity, and interpretation. *)
val c_ProvenanceStatement : Iri.t

(** The set of tags, constructed according to RFC 1766, for the identification of languages. *)
val dt_RFC1766 : Iri.t

(** The set of tags constructed according to RFC 3066 for the identification of languages. *)
val dt_RFC3066 : Iri.t

(** The set of tags constructed according to RFC 4646 for the identification of languages. *)
val dt_RFC4646 : Iri.t

(** The set of tags constructed according to RFC 5646 for the identification of languages. *)
val dt_RFC5646 : Iri.t

(** A statement about the intellectual property rights (IPR) held in or over a Resource, a legal document giving official permission to do something with a resource, or a statement about access rights. *)
val c_RightsStatement : Iri.t

(** A dimension or extent, or a time taken to play or execute. *)
val c_SizeOrDuration : Iri.t

(** A basis for comparison; a reference point against which other things can be evaluated. *)
val c_Standard : Iri.t

(** The set of identifiers constructed according to the generic syntax for Uniform Resource Identifiers as specified by the Internet Engineering Task Force. *)
val dt_URI : Iri.t

(** The set of dates and times constructed according to the W3C Date and Time Formats Specification. *)
val dt_W3CDTF : Iri.t

(** A summary of the resource. *)
val abstract : Iri.t

(** Information about who can access the resource or an indication of its security status. *)
val accessRights : Iri.t

(** The method by which items are added to a collection. *)
val accrualMethod : Iri.t

(** The frequency with which items are added to a collection. *)
val accrualPeriodicity : Iri.t

(** The policy governing the addition of items to a collection. *)
val accrualPolicy : Iri.t

(** An alternative name for the resource. *)
val alternative : Iri.t

(** A class of entity for whom the resource is intended or useful. *)
val audience : Iri.t

(** Date (often a range) that the resource became or will become available. *)
val available : Iri.t

(** A bibliographic reference for the resource. *)
val bibliographicCitation : Iri.t

(** An established standard to which the described resource conforms. *)
val conformsTo : Iri.t

(** An entity responsible for making contributions to the resource. *)
val contributor : Iri.t

(** The spatial or temporal topic of the resource, the spatial applicability of the resource, or the jurisdiction under which the resource is relevant. *)
val coverage : Iri.t

(** Date of creation of the resource. *)
val created : Iri.t

(** An entity primarily responsible for making the resource. *)
val creator : Iri.t

(** A point or period of time associated with an event in the lifecycle of the resource. *)
val date : Iri.t

(** Date of acceptance of the resource. *)
val dateAccepted : Iri.t

(** Date of copyright. *)
val dateCopyrighted : Iri.t

(** Date of submission of the resource. *)
val dateSubmitted : Iri.t

(** An account of the resource. *)
val description : Iri.t

(** A class of entity, defined in terms of progression through an educational or training context, for which the described resource is intended. *)
val educationLevel : Iri.t

(** The size or duration of the resource. *)
val extent : Iri.t

(** The file format, physical medium, or dimensions of the resource. *)
val format : Iri.t

(** A related resource that is substantially the same as the pre-existing described resource, but in another format. *)
val hasFormat : Iri.t

(** A related resource that is included either physically or logically in the described resource. *)
val hasPart : Iri.t

(** A related resource that is a version, edition, or adaptation of the described resource. *)
val hasVersion : Iri.t

(** An unambiguous reference to the resource within a given context. *)
val identifier : Iri.t

(** A process, used to engender knowledge, attitudes and skills, that the described resource is designed to support. *)
val instructionalMethod : Iri.t

(** A related resource that is substantially the same as the described resource, but in another format. *)
val isFormatOf : Iri.t

(** A related resource in which the described resource is physically or logically included. *)
val isPartOf : Iri.t

(** A related resource that references, cites, or otherwise points to the described resource. *)
val isReferencedBy : Iri.t

(** A related resource that supplants, displaces, or supersedes the described resource. *)
val isReplacedBy : Iri.t

(** A related resource that requires the described resource to support its function, delivery, or coherence. *)
val isRequiredBy : Iri.t

(** A related resource of which the described resource is a version, edition, or adaptation. *)
val isVersionOf : Iri.t

(** Date of formal issuance (e.g., publication) of the resource. *)
val issued : Iri.t

(** A language of the resource. *)
val language : Iri.t

(** A legal document giving official permission to do something with the resource. *)
val license : Iri.t

(** An entity that mediates access to the resource and for whom the resource is intended or useful. *)
val mediator : Iri.t

(** The material or physical carrier of the resource. *)
val medium : Iri.t

(** Date on which the resource was changed. *)
val modified : Iri.t

(** A statement of any changes in ownership and custody of the resource since its creation that are significant for its authenticity, integrity, and interpretation. *)
val provenance : Iri.t

(** An entity responsible for making the resource available. *)
val publisher : Iri.t

(** A related resource that is referenced, cited, or otherwise pointed to by the described resource. *)
val references : Iri.t

(** A related resource. *)
val relation : Iri.t

(** A related resource that is supplanted, displaced, or superseded by the described resource. *)
val replaces : Iri.t

(** A related resource that is required by the described resource to support its function, delivery, or coherence. *)
val requires : Iri.t

(** Information about rights held in and over the resource. *)
val rights : Iri.t

(** A person or organization owning or managing rights over the resource. *)
val rightsHolder : Iri.t

(** A related resource from which the described resource is derived. *)
val source : Iri.t

(** Spatial characteristics of the resource. *)
val spatial : Iri.t

(** The topic of the resource. *)
val subject : Iri.t

(** A list of subunits of the resource. *)
val tableOfContents : Iri.t

(** Temporal characteristics of the resource. *)
val temporal : Iri.t

(** A name given to the resource. *)
val title : Iri.t

(** The nature or genre of the resource. *)
val type_ : Iri.t

(** Date (often a range) of validity of a resource. *)
val valid : Iri.t


module Open : sig
  (** A resource that acts or has the power to act. *)
  val dc_c_Agent : Iri.t

  (** A group of agents. *)
  val dc_c_AgentClass : Iri.t

  (** A book, article, or other documentary resource. *)
  val dc_c_BibliographicResource : Iri.t

  (** The set of regions in space defined by their geographic coordinates according to the DCMI Box Encoding Scheme. *)
  val dc_dt_Box : Iri.t

  (** A digital resource format. *)
  val dc_c_FileFormat : Iri.t

  (** A rate at which something recurs. *)
  val dc_c_Frequency : Iri.t

  (** The set of codes listed in ISO 3166-1 for the representation of names of countries. *)
  val dc_dt_ISO3166 : Iri.t

  (** The three-letter alphabetic codes listed in ISO639-2 for the representation of names of languages. *)
  val dc_dt_ISO639_2 : Iri.t

  (** The set of three-letter codes listed in ISO 639-3 for the representation of names of languages. *)
  val dc_dt_ISO639_3 : Iri.t

  (** The extent or range of judicial, law enforcement, or other authority. *)
  val dc_c_Jurisdiction : Iri.t

  (** A legal document giving official permission to do something with a Resource. *)
  val dc_c_LicenseDocument : Iri.t

  (** A system of signs, symbols, sounds, gestures, or rules used in communication. *)
  val dc_c_LinguisticSystem : Iri.t

  (** A spatial region or named place. *)
  val dc_c_Location : Iri.t

  (** A location, period of time, or jurisdiction. *)
  val dc_c_LocationPeriodOrJurisdiction : Iri.t

  (** A file format or physical medium. *)
  val dc_c_MediaType : Iri.t

  (** A media type or extent. *)
  val dc_c_MediaTypeOrExtent : Iri.t

  (** A method by which resources are added to a collection. *)
  val dc_c_MethodOfAccrual : Iri.t

  (** A process that is used to engender knowledge, attitudes, and skills. *)
  val dc_c_MethodOfInstruction : Iri.t

  (** The set of time intervals defined by their limits according to the DCMI Period Encoding Scheme. *)
  val dc_dt_Period : Iri.t

  (** An interval of time that is named or defined by its start and end dates. *)
  val dc_c_PeriodOfTime : Iri.t

  (** A physical material or carrier. *)
  val dc_c_PhysicalMedium : Iri.t

  (** A material thing. *)
  val dc_c_PhysicalResource : Iri.t

  (** The set of points in space defined by their geographic coordinates according to the DCMI Point Encoding Scheme. *)
  val dc_dt_Point : Iri.t

  (** A plan or course of action by an authority, intended to influence and determine decisions, actions, and other matters. *)
  val dc_c_Policy : Iri.t

  (** A statement of any changes in ownership and custody of a resource since its creation that are significant for its authenticity, integrity, and interpretation. *)
  val dc_c_ProvenanceStatement : Iri.t

  (** The set of tags, constructed according to RFC 1766, for the identification of languages. *)
  val dc_dt_RFC1766 : Iri.t

  (** The set of tags constructed according to RFC 3066 for the identification of languages. *)
  val dc_dt_RFC3066 : Iri.t

  (** The set of tags constructed according to RFC 4646 for the identification of languages. *)
  val dc_dt_RFC4646 : Iri.t

  (** The set of tags constructed according to RFC 5646 for the identification of languages. *)
  val dc_dt_RFC5646 : Iri.t

  (** A statement about the intellectual property rights (IPR) held in or over a Resource, a legal document giving official permission to do something with a resource, or a statement about access rights. *)
  val dc_c_RightsStatement : Iri.t

  (** A dimension or extent, or a time taken to play or execute. *)
  val dc_c_SizeOrDuration : Iri.t

  (** A basis for comparison; a reference point against which other things can be evaluated. *)
  val dc_c_Standard : Iri.t

  (** The set of identifiers constructed according to the generic syntax for Uniform Resource Identifiers as specified by the Internet Engineering Task Force. *)
  val dc_dt_URI : Iri.t

  (** The set of dates and times constructed according to the W3C Date and Time Formats Specification. *)
  val dc_dt_W3CDTF : Iri.t

  (** A summary of the resource. *)
  val dc_abstract : Iri.t

  (** Information about who can access the resource or an indication of its security status. *)
  val dc_accessRights : Iri.t

  (** The method by which items are added to a collection. *)
  val dc_accrualMethod : Iri.t

  (** The frequency with which items are added to a collection. *)
  val dc_accrualPeriodicity : Iri.t

  (** The policy governing the addition of items to a collection. *)
  val dc_accrualPolicy : Iri.t

  (** An alternative name for the resource. *)
  val dc_alternative : Iri.t

  (** A class of entity for whom the resource is intended or useful. *)
  val dc_audience : Iri.t

  (** Date (often a range) that the resource became or will become available. *)
  val dc_available : Iri.t

  (** A bibliographic reference for the resource. *)
  val dc_bibliographicCitation : Iri.t

  (** An established standard to which the described resource conforms. *)
  val dc_conformsTo : Iri.t

  (** An entity responsible for making contributions to the resource. *)
  val dc_contributor : Iri.t

  (** The spatial or temporal topic of the resource, the spatial applicability of the resource, or the jurisdiction under which the resource is relevant. *)
  val dc_coverage : Iri.t

  (** Date of creation of the resource. *)
  val dc_created : Iri.t

  (** An entity primarily responsible for making the resource. *)
  val dc_creator : Iri.t

  (** A point or period of time associated with an event in the lifecycle of the resource. *)
  val dc_date : Iri.t

  (** Date of acceptance of the resource. *)
  val dc_dateAccepted : Iri.t

  (** Date of copyright. *)
  val dc_dateCopyrighted : Iri.t

  (** Date of submission of the resource. *)
  val dc_dateSubmitted : Iri.t

  (** An account of the resource. *)
  val dc_description : Iri.t

  (** A class of entity, defined in terms of progression through an educational or training context, for which the described resource is intended. *)
  val dc_educationLevel : Iri.t

  (** The size or duration of the resource. *)
  val dc_extent : Iri.t

  (** The file format, physical medium, or dimensions of the resource. *)
  val dc_format : Iri.t

  (** A related resource that is substantially the same as the pre-existing described resource, but in another format. *)
  val dc_hasFormat : Iri.t

  (** A related resource that is included either physically or logically in the described resource. *)
  val dc_hasPart : Iri.t

  (** A related resource that is a version, edition, or adaptation of the described resource. *)
  val dc_hasVersion : Iri.t

  (** An unambiguous reference to the resource within a given context. *)
  val dc_identifier : Iri.t

  (** A process, used to engender knowledge, attitudes and skills, that the described resource is designed to support. *)
  val dc_instructionalMethod : Iri.t

  (** A related resource that is substantially the same as the described resource, but in another format. *)
  val dc_isFormatOf : Iri.t

  (** A related resource in which the described resource is physically or logically included. *)
  val dc_isPartOf : Iri.t

  (** A related resource that references, cites, or otherwise points to the described resource. *)
  val dc_isReferencedBy : Iri.t

  (** A related resource that supplants, displaces, or supersedes the described resource. *)
  val dc_isReplacedBy : Iri.t

  (** A related resource that requires the described resource to support its function, delivery, or coherence. *)
  val dc_isRequiredBy : Iri.t

  (** A related resource of which the described resource is a version, edition, or adaptation. *)
  val dc_isVersionOf : Iri.t

  (** Date of formal issuance (e.g., publication) of the resource. *)
  val dc_issued : Iri.t

  (** A language of the resource. *)
  val dc_language : Iri.t

  (** A legal document giving official permission to do something with the resource. *)
  val dc_license : Iri.t

  (** An entity that mediates access to the resource and for whom the resource is intended or useful. *)
  val dc_mediator : Iri.t

  (** The material or physical carrier of the resource. *)
  val dc_medium : Iri.t

  (** Date on which the resource was changed. *)
  val dc_modified : Iri.t

  (** A statement of any changes in ownership and custody of the resource since its creation that are significant for its authenticity, integrity, and interpretation. *)
  val dc_provenance : Iri.t

  (** An entity responsible for making the resource available. *)
  val dc_publisher : Iri.t

  (** A related resource that is referenced, cited, or otherwise pointed to by the described resource. *)
  val dc_references : Iri.t

  (** A related resource. *)
  val dc_relation : Iri.t

  (** A related resource that is supplanted, displaced, or superseded by the described resource. *)
  val dc_replaces : Iri.t

  (** A related resource that is required by the described resource to support its function, delivery, or coherence. *)
  val dc_requires : Iri.t

  (** Information about rights held in and over the resource. *)
  val dc_rights : Iri.t

  (** A person or organization owning or managing rights over the resource. *)
  val dc_rightsHolder : Iri.t

  (** A related resource from which the described resource is derived. *)
  val dc_source : Iri.t

  (** Spatial characteristics of the resource. *)
  val dc_spatial : Iri.t

  (** The topic of the resource. *)
  val dc_subject : Iri.t

  (** A list of subunits of the resource. *)
  val dc_tableOfContents : Iri.t

  (** Temporal characteristics of the resource. *)
  val dc_temporal : Iri.t

  (** A name given to the resource. *)
  val dc_title : Iri.t

  (** The nature or genre of the resource. *)
  val dc_type : Iri.t

  (** Date (often a range) of validity of a resource. *)
  val dc_valid : Iri.t

end

class from : ?sub: Rdf_term.term -> Rdf_graph.graph ->
  object
    method abstract : Rdf_term.term list
    method abstract_opt : Rdf_term.term option
    method abstract_iris : Iri.t list
    method abstract_opt_iri : Iri.t option
    method accessRights : Rdf_term.term list
    method accessRights_opt : Rdf_term.term option
    method accessRights_iris : Iri.t list
    method accessRights_opt_iri : Iri.t option
    method accrualMethod : Rdf_term.term list
    method accrualMethod_opt : Rdf_term.term option
    method accrualMethod_iris : Iri.t list
    method accrualMethod_opt_iri : Iri.t option
    method accrualPeriodicity : Rdf_term.term list
    method accrualPeriodicity_opt : Rdf_term.term option
    method accrualPeriodicity_iris : Iri.t list
    method accrualPeriodicity_opt_iri : Iri.t option
    method accrualPolicy : Rdf_term.term list
    method accrualPolicy_opt : Rdf_term.term option
    method accrualPolicy_iris : Iri.t list
    method accrualPolicy_opt_iri : Iri.t option
    method alternative : Rdf_term.literal list
    method alternative_opt : Rdf_term.literal option
    method audience : Rdf_term.term list
    method audience_opt : Rdf_term.term option
    method audience_iris : Iri.t list
    method audience_opt_iri : Iri.t option
    method available : Rdf_term.literal list
    method available_opt : Rdf_term.literal option
    method bibliographicCitation : Rdf_term.literal list
    method bibliographicCitation_opt : Rdf_term.literal option
    method conformsTo : Rdf_term.term list
    method conformsTo_opt : Rdf_term.term option
    method conformsTo_iris : Iri.t list
    method conformsTo_opt_iri : Iri.t option
    method contributor : Rdf_term.term list
    method contributor_opt : Rdf_term.term option
    method contributor_iris : Iri.t list
    method contributor_opt_iri : Iri.t option
    method coverage : Rdf_term.term list
    method coverage_opt : Rdf_term.term option
    method coverage_iris : Iri.t list
    method coverage_opt_iri : Iri.t option
    method created : Rdf_term.literal list
    method created_opt : Rdf_term.literal option
    method creator : Rdf_term.term list
    method creator_opt : Rdf_term.term option
    method creator_iris : Iri.t list
    method creator_opt_iri : Iri.t option
    method date : Rdf_term.literal list
    method date_opt : Rdf_term.literal option
    method dateAccepted : Rdf_term.literal list
    method dateAccepted_opt : Rdf_term.literal option
    method dateCopyrighted : Rdf_term.literal list
    method dateCopyrighted_opt : Rdf_term.literal option
    method dateSubmitted : Rdf_term.literal list
    method dateSubmitted_opt : Rdf_term.literal option
    method description : Rdf_term.term list
    method description_opt : Rdf_term.term option
    method description_iris : Iri.t list
    method description_opt_iri : Iri.t option
    method educationLevel : Rdf_term.term list
    method educationLevel_opt : Rdf_term.term option
    method educationLevel_iris : Iri.t list
    method educationLevel_opt_iri : Iri.t option
    method extent : Rdf_term.term list
    method extent_opt : Rdf_term.term option
    method extent_iris : Iri.t list
    method extent_opt_iri : Iri.t option
    method format : Rdf_term.term list
    method format_opt : Rdf_term.term option
    method format_iris : Iri.t list
    method format_opt_iri : Iri.t option
    method hasFormat : Rdf_term.term list
    method hasFormat_opt : Rdf_term.term option
    method hasFormat_iris : Iri.t list
    method hasFormat_opt_iri : Iri.t option
    method hasPart : Rdf_term.term list
    method hasPart_opt : Rdf_term.term option
    method hasPart_iris : Iri.t list
    method hasPart_opt_iri : Iri.t option
    method hasVersion : Rdf_term.term list
    method hasVersion_opt : Rdf_term.term option
    method hasVersion_iris : Iri.t list
    method hasVersion_opt_iri : Iri.t option
    method identifier : Rdf_term.literal list
    method identifier_opt : Rdf_term.literal option
    method instructionalMethod : Rdf_term.term list
    method instructionalMethod_opt : Rdf_term.term option
    method instructionalMethod_iris : Iri.t list
    method instructionalMethod_opt_iri : Iri.t option
    method isFormatOf : Rdf_term.term list
    method isFormatOf_opt : Rdf_term.term option
    method isFormatOf_iris : Iri.t list
    method isFormatOf_opt_iri : Iri.t option
    method isPartOf : Rdf_term.term list
    method isPartOf_opt : Rdf_term.term option
    method isPartOf_iris : Iri.t list
    method isPartOf_opt_iri : Iri.t option
    method isReferencedBy : Rdf_term.term list
    method isReferencedBy_opt : Rdf_term.term option
    method isReferencedBy_iris : Iri.t list
    method isReferencedBy_opt_iri : Iri.t option
    method isReplacedBy : Rdf_term.term list
    method isReplacedBy_opt : Rdf_term.term option
    method isReplacedBy_iris : Iri.t list
    method isReplacedBy_opt_iri : Iri.t option
    method isRequiredBy : Rdf_term.term list
    method isRequiredBy_opt : Rdf_term.term option
    method isRequiredBy_iris : Iri.t list
    method isRequiredBy_opt_iri : Iri.t option
    method isVersionOf : Rdf_term.term list
    method isVersionOf_opt : Rdf_term.term option
    method isVersionOf_iris : Iri.t list
    method isVersionOf_opt_iri : Iri.t option
    method issued : Rdf_term.literal list
    method issued_opt : Rdf_term.literal option
    method language : Rdf_term.term list
    method language_opt : Rdf_term.term option
    method language_iris : Iri.t list
    method language_opt_iri : Iri.t option
    method license : Rdf_term.term list
    method license_opt : Rdf_term.term option
    method license_iris : Iri.t list
    method license_opt_iri : Iri.t option
    method mediator : Rdf_term.term list
    method mediator_opt : Rdf_term.term option
    method mediator_iris : Iri.t list
    method mediator_opt_iri : Iri.t option
    method medium : Rdf_term.term list
    method medium_opt : Rdf_term.term option
    method medium_iris : Iri.t list
    method medium_opt_iri : Iri.t option
    method modified : Rdf_term.literal list
    method modified_opt : Rdf_term.literal option
    method provenance : Rdf_term.term list
    method provenance_opt : Rdf_term.term option
    method provenance_iris : Iri.t list
    method provenance_opt_iri : Iri.t option
    method publisher : Rdf_term.term list
    method publisher_opt : Rdf_term.term option
    method publisher_iris : Iri.t list
    method publisher_opt_iri : Iri.t option
    method references : Rdf_term.term list
    method references_opt : Rdf_term.term option
    method references_iris : Iri.t list
    method references_opt_iri : Iri.t option
    method relation : Rdf_term.term list
    method relation_opt : Rdf_term.term option
    method relation_iris : Iri.t list
    method relation_opt_iri : Iri.t option
    method replaces : Rdf_term.term list
    method replaces_opt : Rdf_term.term option
    method replaces_iris : Iri.t list
    method replaces_opt_iri : Iri.t option
    method requires : Rdf_term.term list
    method requires_opt : Rdf_term.term option
    method requires_iris : Iri.t list
    method requires_opt_iri : Iri.t option
    method rights : Rdf_term.term list
    method rights_opt : Rdf_term.term option
    method rights_iris : Iri.t list
    method rights_opt_iri : Iri.t option
    method rightsHolder : Rdf_term.term list
    method rightsHolder_opt : Rdf_term.term option
    method rightsHolder_iris : Iri.t list
    method rightsHolder_opt_iri : Iri.t option
    method source : Rdf_term.term list
    method source_opt : Rdf_term.term option
    method source_iris : Iri.t list
    method source_opt_iri : Iri.t option
    method spatial : Rdf_term.term list
    method spatial_opt : Rdf_term.term option
    method spatial_iris : Iri.t list
    method spatial_opt_iri : Iri.t option
    method subject : Rdf_term.term list
    method subject_opt : Rdf_term.term option
    method subject_iris : Iri.t list
    method subject_opt_iri : Iri.t option
    method tableOfContents : Rdf_term.term list
    method tableOfContents_opt : Rdf_term.term option
    method tableOfContents_iris : Iri.t list
    method tableOfContents_opt_iri : Iri.t option
    method temporal : Rdf_term.term list
    method temporal_opt : Rdf_term.term option
    method temporal_iris : Iri.t list
    method temporal_opt_iri : Iri.t option
    method title : Rdf_term.literal list
    method title_opt : Rdf_term.literal option
    method type_ : Rdf_term.term list
    method type__opt : Rdf_term.term option
    method type__iris : Iri.t list
    method type__opt_iri : Iri.t option
    method valid : Rdf_term.literal list
    method valid_opt : Rdf_term.literal option
  end

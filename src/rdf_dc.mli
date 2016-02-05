(** Elements of [http://purl.org/dc/terms/] *)

(** [http://purl.org/dc/terms/] *)
val dc : Iri.t
val dc_ : string -> Iri.t

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

(** A resource that acts or has the power to act. *)
val agent : Iri.t

(** A group of agents. *)
val agentClass : Iri.t

(** An alternative name for the resource. *)
val alternative : Iri.t

(** A class of entity for whom the resource is intended or useful. *)
val audience : Iri.t

(** Date (often a range) that the resource became or will become available. *)
val available : Iri.t

(** A bibliographic reference for the resource. *)
val bibliographicCitation : Iri.t

(** A book, article, or other documentary resource. *)
val bibliographicResource : Iri.t

(** The set of regions in space defined by their geographic coordinates according to the DCMI Box Encoding Scheme. *)
val box : Iri.t

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

(** A digital resource format. *)
val fileFormat : Iri.t

(** The file format, physical medium, or dimensions of the resource. *)
val format : Iri.t

(** A rate at which something recurs. *)
val frequency : Iri.t

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

(** The set of codes listed in ISO 3166-1 for the representation of names of countries. *)
val iSO3166 : Iri.t

(** The three-letter alphabetic codes listed in ISO639-2 for the representation of names of languages. *)
val iSO639_2 : Iri.t

(** The set of three-letter codes listed in ISO 639-3 for the representation of names of languages. *)
val iSO639_3 : Iri.t

(** A related resource in which the described resource is physically or logically included. *)
val isPartOf : Iri.t

(** A related resource that references, cites, or otherwise points to the described resource. *)
val isReferencedBy : Iri.t

(** A related resource that supplants, displaces, or supersedes the described resource. *)
val isReplacedBy : Iri.t

(** A related resource that requires the described resource to support its function, delivery, or coherence. *)
val isRequiredBy : Iri.t

(** Date of formal issuance (e.g., publication) of the resource. *)
val issued : Iri.t

(** A related resource of which the described resource is a version, edition, or adaptation. *)
val isVersionOf : Iri.t

(** The extent or range of judicial, law enforcement, or other authority. *)
val jurisdiction : Iri.t

(** A language of the resource. *)
val language : Iri.t

(** A legal document giving official permission to do something with the resource. *)
val license : Iri.t

(** A legal document giving official permission to do something with a Resource. *)
val licenseDocument : Iri.t

(** A system of signs, symbols, sounds, gestures, or rules used in communication. *)
val linguisticSystem : Iri.t

(** A spatial region or named place. *)
val location : Iri.t

(** A location, period of time, or jurisdiction. *)
val locationPeriodOrJurisdiction : Iri.t

(** An entity that mediates access to the resource and for whom the resource is intended or useful. *)
val mediator : Iri.t

(** A file format or physical medium. *)
val mediaType : Iri.t

(** A media type or extent. *)
val mediaTypeOrExtent : Iri.t

(** The material or physical carrier of the resource. *)
val medium : Iri.t

(** A method by which resources are added to a collection. *)
val methodOfAccrual : Iri.t

(** A process that is used to engender knowledge, attitudes, and skills. *)
val methodOfInstruction : Iri.t

(** Date on which the resource was changed. *)
val modified : Iri.t

(** The set of time intervals defined by their limits according to the DCMI Period Encoding Scheme. *)
val period : Iri.t

(** An interval of time that is named or defined by its start and end dates. *)
val periodOfTime : Iri.t

(** A physical material or carrier. *)
val physicalMedium : Iri.t

(** A material thing. *)
val physicalResource : Iri.t

(** The set of points in space defined by their geographic coordinates according to the DCMI Point Encoding Scheme. *)
val point : Iri.t

(** A plan or course of action by an authority, intended to influence and determine decisions, actions, and other matters. *)
val policy : Iri.t

(** A statement of any changes in ownership and custody of the resource since its creation that are significant for its authenticity, integrity, and interpretation. *)
val provenance : Iri.t

(** A statement of any changes in ownership and custody of a resource since its creation that are significant for its authenticity, integrity, and interpretation. *)
val provenanceStatement : Iri.t

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

(** The set of tags, constructed according to RFC 1766, for the identification of languages. *)
val rFC1766 : Iri.t

(** The set of tags constructed according to RFC 3066 for the identification of languages. *)
val rFC3066 : Iri.t

(** The set of tags constructed according to RFC 4646 for the identification of languages. *)
val rFC4646 : Iri.t

(** The set of tags constructed according to RFC 5646 for the identification of languages. *)
val rFC5646 : Iri.t

(** Information about rights held in and over the resource. *)
val rights : Iri.t

(** A person or organization owning or managing rights over the resource. *)
val rightsHolder : Iri.t

(** A statement about the intellectual property rights (IPR) held in or over a Resource, a legal document giving official permission to do something with a resource, or a statement about access rights. *)
val rightsStatement : Iri.t

(** A dimension or extent, or a time taken to play or execute. *)
val sizeOrDuration : Iri.t

(** A related resource from which the described resource is derived. *)
val source : Iri.t

(** Spatial characteristics of the resource. *)
val spatial : Iri.t

(** A basis for comparison; a reference point against which other things can be evaluated. *)
val standard : Iri.t

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

(** The set of identifiers constructed according to the generic syntax for Uniform Resource Identifiers as specified by the Internet Engineering Task Force. *)
val uRI : Iri.t

(** Date (often a range) of validity of a resource. *)
val valid : Iri.t

(** The set of dates and times constructed according to the W3C Date and Time Formats Specification. *)
val w3CDTF : Iri.t


module Open : sig
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

  (** A resource that acts or has the power to act. *)
  val dc_agent : Iri.t

  (** A group of agents. *)
  val dc_agentClass : Iri.t

  (** An alternative name for the resource. *)
  val dc_alternative : Iri.t

  (** A class of entity for whom the resource is intended or useful. *)
  val dc_audience : Iri.t

  (** Date (often a range) that the resource became or will become available. *)
  val dc_available : Iri.t

  (** A bibliographic reference for the resource. *)
  val dc_bibliographicCitation : Iri.t

  (** A book, article, or other documentary resource. *)
  val dc_bibliographicResource : Iri.t

  (** The set of regions in space defined by their geographic coordinates according to the DCMI Box Encoding Scheme. *)
  val dc_box : Iri.t

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

  (** A digital resource format. *)
  val dc_fileFormat : Iri.t

  (** The file format, physical medium, or dimensions of the resource. *)
  val dc_format : Iri.t

  (** A rate at which something recurs. *)
  val dc_frequency : Iri.t

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

  (** The set of codes listed in ISO 3166-1 for the representation of names of countries. *)
  val dc_iSO3166 : Iri.t

  (** The three-letter alphabetic codes listed in ISO639-2 for the representation of names of languages. *)
  val dc_iSO639_2 : Iri.t

  (** The set of three-letter codes listed in ISO 639-3 for the representation of names of languages. *)
  val dc_iSO639_3 : Iri.t

  (** A related resource in which the described resource is physically or logically included. *)
  val dc_isPartOf : Iri.t

  (** A related resource that references, cites, or otherwise points to the described resource. *)
  val dc_isReferencedBy : Iri.t

  (** A related resource that supplants, displaces, or supersedes the described resource. *)
  val dc_isReplacedBy : Iri.t

  (** A related resource that requires the described resource to support its function, delivery, or coherence. *)
  val dc_isRequiredBy : Iri.t

  (** Date of formal issuance (e.g., publication) of the resource. *)
  val dc_issued : Iri.t

  (** A related resource of which the described resource is a version, edition, or adaptation. *)
  val dc_isVersionOf : Iri.t

  (** The extent or range of judicial, law enforcement, or other authority. *)
  val dc_jurisdiction : Iri.t

  (** A language of the resource. *)
  val dc_language : Iri.t

  (** A legal document giving official permission to do something with the resource. *)
  val dc_license : Iri.t

  (** A legal document giving official permission to do something with a Resource. *)
  val dc_licenseDocument : Iri.t

  (** A system of signs, symbols, sounds, gestures, or rules used in communication. *)
  val dc_linguisticSystem : Iri.t

  (** A spatial region or named place. *)
  val dc_location : Iri.t

  (** A location, period of time, or jurisdiction. *)
  val dc_locationPeriodOrJurisdiction : Iri.t

  (** An entity that mediates access to the resource and for whom the resource is intended or useful. *)
  val dc_mediator : Iri.t

  (** A file format or physical medium. *)
  val dc_mediaType : Iri.t

  (** A media type or extent. *)
  val dc_mediaTypeOrExtent : Iri.t

  (** The material or physical carrier of the resource. *)
  val dc_medium : Iri.t

  (** A method by which resources are added to a collection. *)
  val dc_methodOfAccrual : Iri.t

  (** A process that is used to engender knowledge, attitudes, and skills. *)
  val dc_methodOfInstruction : Iri.t

  (** Date on which the resource was changed. *)
  val dc_modified : Iri.t

  (** The set of time intervals defined by their limits according to the DCMI Period Encoding Scheme. *)
  val dc_period : Iri.t

  (** An interval of time that is named or defined by its start and end dates. *)
  val dc_periodOfTime : Iri.t

  (** A physical material or carrier. *)
  val dc_physicalMedium : Iri.t

  (** A material thing. *)
  val dc_physicalResource : Iri.t

  (** The set of points in space defined by their geographic coordinates according to the DCMI Point Encoding Scheme. *)
  val dc_point : Iri.t

  (** A plan or course of action by an authority, intended to influence and determine decisions, actions, and other matters. *)
  val dc_policy : Iri.t

  (** A statement of any changes in ownership and custody of the resource since its creation that are significant for its authenticity, integrity, and interpretation. *)
  val dc_provenance : Iri.t

  (** A statement of any changes in ownership and custody of a resource since its creation that are significant for its authenticity, integrity, and interpretation. *)
  val dc_provenanceStatement : Iri.t

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

  (** The set of tags, constructed according to RFC 1766, for the identification of languages. *)
  val dc_rFC1766 : Iri.t

  (** The set of tags constructed according to RFC 3066 for the identification of languages. *)
  val dc_rFC3066 : Iri.t

  (** The set of tags constructed according to RFC 4646 for the identification of languages. *)
  val dc_rFC4646 : Iri.t

  (** The set of tags constructed according to RFC 5646 for the identification of languages. *)
  val dc_rFC5646 : Iri.t

  (** Information about rights held in and over the resource. *)
  val dc_rights : Iri.t

  (** A person or organization owning or managing rights over the resource. *)
  val dc_rightsHolder : Iri.t

  (** A statement about the intellectual property rights (IPR) held in or over a Resource, a legal document giving official permission to do something with a resource, or a statement about access rights. *)
  val dc_rightsStatement : Iri.t

  (** A dimension or extent, or a time taken to play or execute. *)
  val dc_sizeOrDuration : Iri.t

  (** A related resource from which the described resource is derived. *)
  val dc_source : Iri.t

  (** Spatial characteristics of the resource. *)
  val dc_spatial : Iri.t

  (** A basis for comparison; a reference point against which other things can be evaluated. *)
  val dc_standard : Iri.t

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

  (** The set of identifiers constructed according to the generic syntax for Uniform Resource Identifiers as specified by the Internet Engineering Task Force. *)
  val dc_uRI : Iri.t

  (** Date (often a range) of validity of a resource. *)
  val dc_valid : Iri.t

  (** The set of dates and times constructed according to the W3C Date and Time Formats Specification. *)
  val dc_w3CDTF : Iri.t

end

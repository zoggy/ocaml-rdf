(** Dublin core. *)

(** {2 Elements of [http://purl.org/dc/elements/1.1/]} *)

(** [http://purl.org/dc/elements/1.1/] *)
val dc : Rdf_iri.iri
val dc_ : string -> Rdf_iri.iri

(** An entity responsible for making contributions to the resource. *)
val dc_contributor : Rdf_iri.iri

(** The spatial or temporal topic of the resource, the spatial applicability of the resource, or the jurisdiction under which the resource is relevant. *)
val dc_coverage : Rdf_iri.iri

(** An entity primarily responsible for making the resource. *)
val dc_creator : Rdf_iri.iri

(** A point or period of time associated with an event in the lifecycle of the resource. *)
val dc_date : Rdf_iri.iri

(** An account of the resource. *)
val dc_description : Rdf_iri.iri

(** The file format, physical medium, or dimensions of the resource. *)
val dc_format : Rdf_iri.iri

(** An unambiguous reference to the resource within a given context. *)
val dc_identifier : Rdf_iri.iri

(** A language of the resource. *)
val dc_language : Rdf_iri.iri

(** An entity responsible for making the resource available. *)
val dc_publisher : Rdf_iri.iri

(** A related resource. *)
val dc_relation : Rdf_iri.iri

(** Information about rights held in and over the resource. *)
val dc_rights : Rdf_iri.iri

(** A related resource from which the described resource is derived. *)
val dc_source : Rdf_iri.iri

(** The topic of the resource. *)
val dc_subject : Rdf_iri.iri

(** A name given to the resource. *)
val dc_title : Rdf_iri.iri

(** The nature or genre of the resource. *)
val dc_type : Rdf_iri.iri

(** {2 Elements of [http://purl.org/dc/terms/]} *)

(** [http://purl.org/dc/terms/] *)
val terms : Rdf_iri.iri
val terms_ : string -> Rdf_iri.iri

(** A summary of the resource. *)
val terms_abstract : Rdf_iri.iri

(** Information about who can access the resource or an indication of its security status. *)
val terms_accessRights : Rdf_iri.iri

(** The method by which items are added to a collection. *)
val terms_accrualMethod : Rdf_iri.iri

(** The frequency with which items are added to a collection. *)
val terms_accrualPeriodicity : Rdf_iri.iri

(** The policy governing the addition of items to a collection. *)
val terms_accrualPolicy : Rdf_iri.iri

(** A resource that acts or has the power to act. *)
val terms_Agent : Rdf_iri.iri

(** A group of agents. *)
val terms_AgentClass : Rdf_iri.iri

(** An alternative name for the resource. *)
val terms_alternative : Rdf_iri.iri

(** A class of entity for whom the resource is intended or useful. *)
val terms_audience : Rdf_iri.iri

(** Date (often a range) that the resource became or will become available. *)
val terms_available : Rdf_iri.iri

(** A bibliographic reference for the resource. *)
val terms_bibliographicCitation : Rdf_iri.iri

(** A book, article, or other documentary resource. *)
val terms_BibliographicResource : Rdf_iri.iri

(** The set of regions in space defined by their geographic coordinates according to the DCMI Box Encoding Scheme. *)
val terms_Box : Rdf_iri.iri

(** An established standard to which the described resource conforms. *)
val terms_conformsTo : Rdf_iri.iri

(** An entity responsible for making contributions to the resource. *)
val terms_contributor : Rdf_iri.iri

(** The spatial or temporal topic of the resource, the spatial applicability of the resource, or the jurisdiction under which the resource is relevant. *)
val terms_coverage : Rdf_iri.iri

(** Date of creation of the resource. *)
val terms_created : Rdf_iri.iri

(** An entity primarily responsible for making the resource. *)
val terms_creator : Rdf_iri.iri

(** A point or period of time associated with an event in the lifecycle of the resource. *)
val terms_date : Rdf_iri.iri

(** Date of acceptance of the resource. *)
val terms_dateAccepted : Rdf_iri.iri

(** Date of copyright. *)
val terms_dateCopyrighted : Rdf_iri.iri

(** Date of submission of the resource. *)
val terms_dateSubmitted : Rdf_iri.iri

(** An account of the resource. *)
val terms_description : Rdf_iri.iri

(** A class of entity, defined in terms of progression through an educational or training context, for which the described resource is intended. *)
val terms_educationLevel : Rdf_iri.iri

(** The size or duration of the resource. *)
val terms_extent : Rdf_iri.iri

(** A digital resource format. *)
val terms_FileFormat : Rdf_iri.iri

(** The file format, physical medium, or dimensions of the resource. *)
val terms_format : Rdf_iri.iri

(** A rate at which something recurs. *)
val terms_Frequency : Rdf_iri.iri

(** A related resource that is substantially the same as the pre-existing described resource, but in another format. *)
val terms_hasFormat : Rdf_iri.iri

(** A related resource that is included either physically or logically in the described resource. *)
val terms_hasPart : Rdf_iri.iri

(** A related resource that is a version, edition, or adaptation of the described resource. *)
val terms_hasVersion : Rdf_iri.iri

(** An unambiguous reference to the resource within a given context. *)
val terms_identifier : Rdf_iri.iri

(** A process, used to engender knowledge, attitudes and skills, that the described resource is designed to support. *)
val terms_instructionalMethod : Rdf_iri.iri

(** A related resource that is substantially the same as the described resource, but in another format. *)
val terms_isFormatOf : Rdf_iri.iri

(** The set of codes listed in ISO 3166-1 for the representation of names of countries. *)
val terms_ISO3166 : Rdf_iri.iri

(** The three-letter alphabetic codes listed in ISO639-2 for the representation of names of languages. *)
val terms_ISO639_2 : Rdf_iri.iri

(** The set of three-letter codes listed in ISO 639-3 for the representation of names of languages. *)
val terms_ISO639_3 : Rdf_iri.iri

(** A related resource in which the described resource is physically or logically included. *)
val terms_isPartOf : Rdf_iri.iri

(** A related resource that references, cites, or otherwise points to the described resource. *)
val terms_isReferencedBy : Rdf_iri.iri

(** A related resource that supplants, displaces, or supersedes the described resource. *)
val terms_isReplacedBy : Rdf_iri.iri

(** A related resource that requires the described resource to support its function, delivery, or coherence. *)
val terms_isRequiredBy : Rdf_iri.iri

(** Date of formal issuance (e.g., publication) of the resource. *)
val terms_issued : Rdf_iri.iri

(** A related resource of which the described resource is a version, edition, or adaptation. *)
val terms_isVersionOf : Rdf_iri.iri

(** The extent or range of judicial, law enforcement, or other authority. *)
val terms_Jurisdiction : Rdf_iri.iri

(** A language of the resource. *)
val terms_language : Rdf_iri.iri

(** A legal document giving official permission to do something with the resource. *)
val terms_license : Rdf_iri.iri

(** A legal document giving official permission to do something with a Resource. *)
val terms_LicenseDocument : Rdf_iri.iri

(** A system of signs, symbols, sounds, gestures, or rules used in communication. *)
val terms_LinguisticSystem : Rdf_iri.iri

(** A spatial region or named place. *)
val terms_Location : Rdf_iri.iri

(** A location, period of time, or jurisdiction. *)
val terms_LocationPeriodOrJurisdiction : Rdf_iri.iri

(** An entity that mediates access to the resource and for whom the resource is intended or useful. *)
val terms_mediator : Rdf_iri.iri

(** A file format or physical medium. *)
val terms_MediaType : Rdf_iri.iri

(** A media type or extent. *)
val terms_MediaTypeOrExtent : Rdf_iri.iri

(** The material or physical carrier of the resource. *)
val terms_medium : Rdf_iri.iri

(** A method by which resources are added to a collection. *)
val terms_MethodOfAccrual : Rdf_iri.iri

(** A process that is used to engender knowledge, attitudes, and skills. *)
val terms_MethodOfInstruction : Rdf_iri.iri

(** Date on which the resource was changed. *)
val terms_modified : Rdf_iri.iri

(** The set of time intervals defined by their limits according to the DCMI Period Encoding Scheme. *)
val terms_Period : Rdf_iri.iri

(** An interval of time that is named or defined by its start and end dates. *)
val terms_PeriodOfTime : Rdf_iri.iri

(** A physical material or carrier. *)
val terms_PhysicalMedium : Rdf_iri.iri

(** A material thing. *)
val terms_PhysicalResource : Rdf_iri.iri

(** The set of points in space defined by their geographic coordinates according to the DCMI Point Encoding Scheme. *)
val terms_Point : Rdf_iri.iri

(** A plan or course of action by an authority, intended to influence and determine decisions, actions, and other matters. *)
val terms_Policy : Rdf_iri.iri

(** A statement of any changes in ownership and custody of the resource since its creation that are significant for its authenticity, integrity, and interpretation. *)
val terms_provenance : Rdf_iri.iri

(** A statement of any changes in ownership and custody of a resource since its creation that are significant for its authenticity, integrity, and interpretation. *)
val terms_ProvenanceStatement : Rdf_iri.iri

(** An entity responsible for making the resource available. *)
val terms_publisher : Rdf_iri.iri

(** A related resource that is referenced, cited, or otherwise pointed to by the described resource. *)
val terms_references : Rdf_iri.iri

(** A related resource. *)
val terms_relation : Rdf_iri.iri

(** A related resource that is supplanted, displaced, or superseded by the described resource. *)
val terms_replaces : Rdf_iri.iri

(** A related resource that is required by the described resource to support its function, delivery, or coherence. *)
val terms_requires : Rdf_iri.iri

(** The set of tags, constructed according to RFC 1766, for the identification of languages. *)
val terms_RFC1766 : Rdf_iri.iri

(** The set of tags constructed according to RFC 3066 for the identification of languages. *)
val terms_RFC3066 : Rdf_iri.iri

(** The set of tags constructed according to RFC 4646 for the identification of languages. *)
val terms_RFC4646 : Rdf_iri.iri

(** The set of tags constructed according to RFC 5646 for the identification of languages. *)
val terms_RFC5646 : Rdf_iri.iri

(** Information about rights held in and over the resource. *)
val terms_rights : Rdf_iri.iri

(** A person or organization owning or managing rights over the resource. *)
val terms_rightsHolder : Rdf_iri.iri

(** A statement about the intellectual property rights (IPR) held in or over a Resource, a legal document giving official permission to do something with a resource, or a statement about access rights. *)
val terms_RightsStatement : Rdf_iri.iri

(** A dimension or extent, or a time taken to play or execute. *)
val terms_SizeOrDuration : Rdf_iri.iri

(** A related resource from which the described resource is derived. *)
val terms_source : Rdf_iri.iri

(** Spatial characteristics of the resource. *)
val terms_spatial : Rdf_iri.iri

(** A basis for comparison; a reference point against which other things can be evaluated. *)
val terms_Standard : Rdf_iri.iri

(** The topic of the resource. *)
val terms_subject : Rdf_iri.iri

(** A list of subunits of the resource. *)
val terms_tableOfContents : Rdf_iri.iri

(** Temporal characteristics of the resource. *)
val terms_temporal : Rdf_iri.iri

(** A name given to the resource. *)
val terms_title : Rdf_iri.iri

(** The nature or genre of the resource. *)
val terms_type : Rdf_iri.iri

(** The set of identifiers constructed according to the generic syntax for Uniform Resource Identifiers as specified by the Internet Engineering Task Force. *)
val terms_URI : Rdf_iri.iri

(** Date (often a range) of validity of a resource. *)
val terms_valid : Rdf_iri.iri

(** The set of dates and times constructed according to the W3C Date and Time Formats Specification. *)
val terms_W3CDTF : Rdf_iri.iri


(*********************************************************************************)
(*                OCaml-RDF                                                      *)
(*                                                                               *)
(*    Copyright (C) 2012-2014 Institut National de Recherche en Informatique     *)
(*    et en Automatique. All rights reserved.                                    *)
(*                                                                               *)
(*    This program is free software; you can redistribute it and/or modify       *)
(*    it under the terms of the GNU Lesser General Public License version        *)
(*    3 as published by the Free Software Foundation.                            *)
(*                                                                               *)
(*    This program is distributed in the hope that it will be useful,            *)
(*    but WITHOUT ANY WARRANTY; without even the implied warranty of             *)
(*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the              *)
(*    GNU General Public License for more details.                               *)
(*                                                                               *)
(*    You should have received a copy of the GNU General Public License          *)
(*    along with this program; if not, write to the Free Software                *)
(*    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA                   *)
(*    02111-1307  USA                                                            *)
(*                                                                               *)
(*    Contact: Maxence.Guesdon@inria.fr                                          *)
(*                                                                               *)
(*********************************************************************************)

(** Dublin core. *)

(** {2 Elements of [http://purl.org/dc/elements/1.1/]} *)

(** [http://purl.org/dc/elements/1.1/] *)
val dc : Iri.iri
val dc_ : string -> Iri.iri

(** An entity responsible for making contributions to the resource. *)
val dc_contributor : Iri.iri

(** The spatial or temporal topic of the resource, the spatial applicability of the resource, or the jurisdiction under which the resource is relevant. *)
val dc_coverage : Iri.iri

(** An entity primarily responsible for making the resource. *)
val dc_creator : Iri.iri

(** A point or period of time associated with an event in the lifecycle of the resource. *)
val dc_date : Iri.iri

(** An account of the resource. *)
val dc_description : Iri.iri

(** The file format, physical medium, or dimensions of the resource. *)
val dc_format : Iri.iri

(** An unambiguous reference to the resource within a given context. *)
val dc_identifier : Iri.iri

(** A language of the resource. *)
val dc_language : Iri.iri

(** An entity responsible for making the resource available. *)
val dc_publisher : Iri.iri

(** A related resource. *)
val dc_relation : Iri.iri

(** Information about rights held in and over the resource. *)
val dc_rights : Iri.iri

(** A related resource from which the described resource is derived. *)
val dc_source : Iri.iri

(** The topic of the resource. *)
val dc_subject : Iri.iri

(** A name given to the resource. *)
val dc_title : Iri.iri

(** The nature or genre of the resource. *)
val dc_type : Iri.iri

(** {2 Elements of [http://purl.org/dc/terms/]} *)

(** [http://purl.org/dc/terms/] *)
val terms : Iri.iri
val terms_ : string -> Iri.iri

(** A summary of the resource. *)
val terms_abstract : Iri.iri

(** Information about who can access the resource or an indication of its security status. *)
val terms_accessRights : Iri.iri

(** The method by which items are added to a collection. *)
val terms_accrualMethod : Iri.iri

(** The frequency with which items are added to a collection. *)
val terms_accrualPeriodicity : Iri.iri

(** The policy governing the addition of items to a collection. *)
val terms_accrualPolicy : Iri.iri

(** A resource that acts or has the power to act. *)
val terms_Agent : Iri.iri

(** A group of agents. *)
val terms_AgentClass : Iri.iri

(** An alternative name for the resource. *)
val terms_alternative : Iri.iri

(** A class of entity for whom the resource is intended or useful. *)
val terms_audience : Iri.iri

(** Date (often a range) that the resource became or will become available. *)
val terms_available : Iri.iri

(** A bibliographic reference for the resource. *)
val terms_bibliographicCitation : Iri.iri

(** A book, article, or other documentary resource. *)
val terms_BibliographicResource : Iri.iri

(** The set of regions in space defined by their geographic coordinates according to the DCMI Box Encoding Scheme. *)
val terms_Box : Iri.iri

(** An established standard to which the described resource conforms. *)
val terms_conformsTo : Iri.iri

(** An entity responsible for making contributions to the resource. *)
val terms_contributor : Iri.iri

(** The spatial or temporal topic of the resource, the spatial applicability of the resource, or the jurisdiction under which the resource is relevant. *)
val terms_coverage : Iri.iri

(** Date of creation of the resource. *)
val terms_created : Iri.iri

(** An entity primarily responsible for making the resource. *)
val terms_creator : Iri.iri

(** A point or period of time associated with an event in the lifecycle of the resource. *)
val terms_date : Iri.iri

(** Date of acceptance of the resource. *)
val terms_dateAccepted : Iri.iri

(** Date of copyright. *)
val terms_dateCopyrighted : Iri.iri

(** Date of submission of the resource. *)
val terms_dateSubmitted : Iri.iri

(** An account of the resource. *)
val terms_description : Iri.iri

(** A class of entity, defined in terms of progression through an educational or training context, for which the described resource is intended. *)
val terms_educationLevel : Iri.iri

(** The size or duration of the resource. *)
val terms_extent : Iri.iri

(** A digital resource format. *)
val terms_FileFormat : Iri.iri

(** The file format, physical medium, or dimensions of the resource. *)
val terms_format : Iri.iri

(** A rate at which something recurs. *)
val terms_Frequency : Iri.iri

(** A related resource that is substantially the same as the pre-existing described resource, but in another format. *)
val terms_hasFormat : Iri.iri

(** A related resource that is included either physically or logically in the described resource. *)
val terms_hasPart : Iri.iri

(** A related resource that is a version, edition, or adaptation of the described resource. *)
val terms_hasVersion : Iri.iri

(** An unambiguous reference to the resource within a given context. *)
val terms_identifier : Iri.iri

(** A process, used to engender knowledge, attitudes and skills, that the described resource is designed to support. *)
val terms_instructionalMethod : Iri.iri

(** A related resource that is substantially the same as the described resource, but in another format. *)
val terms_isFormatOf : Iri.iri

(** The set of codes listed in ISO 3166-1 for the representation of names of countries. *)
val terms_ISO3166 : Iri.iri

(** The three-letter alphabetic codes listed in ISO639-2 for the representation of names of languages. *)
val terms_ISO639_2 : Iri.iri

(** The set of three-letter codes listed in ISO 639-3 for the representation of names of languages. *)
val terms_ISO639_3 : Iri.iri

(** A related resource in which the described resource is physically or logically included. *)
val terms_isPartOf : Iri.iri

(** A related resource that references, cites, or otherwise points to the described resource. *)
val terms_isReferencedBy : Iri.iri

(** A related resource that supplants, displaces, or supersedes the described resource. *)
val terms_isReplacedBy : Iri.iri

(** A related resource that requires the described resource to support its function, delivery, or coherence. *)
val terms_isRequiredBy : Iri.iri

(** Date of formal issuance (e.g., publication) of the resource. *)
val terms_issued : Iri.iri

(** A related resource of which the described resource is a version, edition, or adaptation. *)
val terms_isVersionOf : Iri.iri

(** The extent or range of judicial, law enforcement, or other authority. *)
val terms_Jurisdiction : Iri.iri

(** A language of the resource. *)
val terms_language : Iri.iri

(** A legal document giving official permission to do something with the resource. *)
val terms_license : Iri.iri

(** A legal document giving official permission to do something with a Resource. *)
val terms_LicenseDocument : Iri.iri

(** A system of signs, symbols, sounds, gestures, or rules used in communication. *)
val terms_LinguisticSystem : Iri.iri

(** A spatial region or named place. *)
val terms_Location : Iri.iri

(** A location, period of time, or jurisdiction. *)
val terms_LocationPeriodOrJurisdiction : Iri.iri

(** An entity that mediates access to the resource and for whom the resource is intended or useful. *)
val terms_mediator : Iri.iri

(** A file format or physical medium. *)
val terms_MediaType : Iri.iri

(** A media type or extent. *)
val terms_MediaTypeOrExtent : Iri.iri

(** The material or physical carrier of the resource. *)
val terms_medium : Iri.iri

(** A method by which resources are added to a collection. *)
val terms_MethodOfAccrual : Iri.iri

(** A process that is used to engender knowledge, attitudes, and skills. *)
val terms_MethodOfInstruction : Iri.iri

(** Date on which the resource was changed. *)
val terms_modified : Iri.iri

(** The set of time intervals defined by their limits according to the DCMI Period Encoding Scheme. *)
val terms_Period : Iri.iri

(** An interval of time that is named or defined by its start and end dates. *)
val terms_PeriodOfTime : Iri.iri

(** A physical material or carrier. *)
val terms_PhysicalMedium : Iri.iri

(** A material thing. *)
val terms_PhysicalResource : Iri.iri

(** The set of points in space defined by their geographic coordinates according to the DCMI Point Encoding Scheme. *)
val terms_Point : Iri.iri

(** A plan or course of action by an authority, intended to influence and determine decisions, actions, and other matters. *)
val terms_Policy : Iri.iri

(** A statement of any changes in ownership and custody of the resource since its creation that are significant for its authenticity, integrity, and interpretation. *)
val terms_provenance : Iri.iri

(** A statement of any changes in ownership and custody of a resource since its creation that are significant for its authenticity, integrity, and interpretation. *)
val terms_ProvenanceStatement : Iri.iri

(** An entity responsible for making the resource available. *)
val terms_publisher : Iri.iri

(** A related resource that is referenced, cited, or otherwise pointed to by the described resource. *)
val terms_references : Iri.iri

(** A related resource. *)
val terms_relation : Iri.iri

(** A related resource that is supplanted, displaced, or superseded by the described resource. *)
val terms_replaces : Iri.iri

(** A related resource that is required by the described resource to support its function, delivery, or coherence. *)
val terms_requires : Iri.iri

(** The set of tags, constructed according to RFC 1766, for the identification of languages. *)
val terms_RFC1766 : Iri.iri

(** The set of tags constructed according to RFC 3066 for the identification of languages. *)
val terms_RFC3066 : Iri.iri

(** The set of tags constructed according to RFC 4646 for the identification of languages. *)
val terms_RFC4646 : Iri.iri

(** The set of tags constructed according to RFC 5646 for the identification of languages. *)
val terms_RFC5646 : Iri.iri

(** Information about rights held in and over the resource. *)
val terms_rights : Iri.iri

(** A person or organization owning or managing rights over the resource. *)
val terms_rightsHolder : Iri.iri

(** A statement about the intellectual property rights (IPR) held in or over a Resource, a legal document giving official permission to do something with a resource, or a statement about access rights. *)
val terms_RightsStatement : Iri.iri

(** A dimension or extent, or a time taken to play or execute. *)
val terms_SizeOrDuration : Iri.iri

(** A related resource from which the described resource is derived. *)
val terms_source : Iri.iri

(** Spatial characteristics of the resource. *)
val terms_spatial : Iri.iri

(** A basis for comparison; a reference point against which other things can be evaluated. *)
val terms_Standard : Iri.iri

(** The topic of the resource. *)
val terms_subject : Iri.iri

(** A list of subunits of the resource. *)
val terms_tableOfContents : Iri.iri

(** Temporal characteristics of the resource. *)
val terms_temporal : Iri.iri

(** A name given to the resource. *)
val terms_title : Iri.iri

(** The nature or genre of the resource. *)
val terms_type : Iri.iri

(** The set of identifiers constructed according to the generic syntax for Uniform Resource Identifiers as specified by the Internet Engineering Task Force. *)
val terms_URI : Iri.iri

(** Date (often a range) of validity of a resource. *)
val terms_valid : Iri.iri

(** The set of dates and times constructed according to the W3C Date and Time Formats Specification. *)
val terms_W3CDTF : Iri.iri


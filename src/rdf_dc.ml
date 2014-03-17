(*********************************************************************************)
(*                OCaml-RDF                                                      *)
(*                                                                               *)
(*    Copyright (C) 2012-2013 Institut National de Recherche en Informatique     *)
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

let dc = Rdf_iri.iri "http://purl.org/dc/elements/1.1/";;
let dc_ = Rdf_iri.append dc;;

let dc_contributor = dc_"contributor" ;;

let dc_coverage = dc_"coverage" ;;

let dc_creator = dc_"creator" ;;

let dc_date = dc_"date" ;;

let dc_description = dc_"description" ;;

let dc_format = dc_"format" ;;

let dc_identifier = dc_"identifier" ;;

let dc_language = dc_"language" ;;

let dc_publisher = dc_"publisher" ;;

let dc_relation = dc_"relation" ;;

let dc_rights = dc_"rights" ;;

let dc_source = dc_"source" ;;

let dc_subject = dc_"subject" ;;

let dc_title = dc_"title" ;;

let dc_type = dc_"type" ;;


let terms = Rdf_iri.iri "http://purl.org/dc/terms/";;
let terms_ = Rdf_iri.append terms;;

let terms_abstract = terms_"abstract" ;;

let terms_accessRights = terms_"accessRights" ;;

let terms_accrualMethod = terms_"accrualMethod" ;;

let terms_accrualPeriodicity = terms_"accrualPeriodicity" ;;

let terms_accrualPolicy = terms_"accrualPolicy" ;;

let terms_Agent = terms_"Agent" ;;

let terms_AgentClass = terms_"AgentClass" ;;

let terms_alternative = terms_"alternative" ;;

let terms_audience = terms_"audience" ;;

let terms_available = terms_"available" ;;

let terms_bibliographicCitation = terms_"bibliographicCitation" ;;

let terms_BibliographicResource = terms_"BibliographicResource" ;;

let terms_Box = terms_"Box" ;;

let terms_conformsTo = terms_"conformsTo" ;;

let terms_contributor = terms_"contributor" ;;

let terms_coverage = terms_"coverage" ;;

let terms_created = terms_"created" ;;

let terms_creator = terms_"creator" ;;

let terms_date = terms_"date" ;;

let terms_dateAccepted = terms_"dateAccepted" ;;

let terms_dateCopyrighted = terms_"dateCopyrighted" ;;

let terms_dateSubmitted = terms_"dateSubmitted" ;;

let terms_description = terms_"description" ;;

let terms_educationLevel = terms_"educationLevel" ;;

let terms_extent = terms_"extent" ;;

let terms_FileFormat = terms_"FileFormat" ;;

let terms_format = terms_"format" ;;

let terms_Frequency = terms_"Frequency" ;;

let terms_hasFormat = terms_"hasFormat" ;;

let terms_hasPart = terms_"hasPart" ;;

let terms_hasVersion = terms_"hasVersion" ;;

let terms_identifier = terms_"identifier" ;;

let terms_instructionalMethod = terms_"instructionalMethod" ;;

let terms_isFormatOf = terms_"isFormatOf" ;;

let terms_ISO3166 = terms_"ISO3166" ;;

let terms_ISO639_2 = terms_"ISO639-2" ;;

let terms_ISO639_3 = terms_"ISO639-3" ;;

let terms_isPartOf = terms_"isPartOf" ;;

let terms_isReferencedBy = terms_"isReferencedBy" ;;

let terms_isReplacedBy = terms_"isReplacedBy" ;;

let terms_isRequiredBy = terms_"isRequiredBy" ;;

let terms_issued = terms_"issued" ;;

let terms_isVersionOf = terms_"isVersionOf" ;;

let terms_Jurisdiction = terms_"Jurisdiction" ;;

let terms_language = terms_"language" ;;

let terms_license = terms_"license" ;;

let terms_LicenseDocument = terms_"LicenseDocument" ;;

let terms_LinguisticSystem = terms_"LinguisticSystem" ;;

let terms_Location = terms_"Location" ;;

let terms_LocationPeriodOrJurisdiction = terms_"LocationPeriodOrJurisdiction" ;;

let terms_mediator = terms_"mediator" ;;

let terms_MediaType = terms_"MediaType" ;;

let terms_MediaTypeOrExtent = terms_"MediaTypeOrExtent" ;;

let terms_medium = terms_"medium" ;;

let terms_MethodOfAccrual = terms_"MethodOfAccrual" ;;

let terms_MethodOfInstruction = terms_"MethodOfInstruction" ;;

let terms_modified = terms_"modified" ;;

let terms_Period = terms_"Period" ;;

let terms_PeriodOfTime = terms_"PeriodOfTime" ;;

let terms_PhysicalMedium = terms_"PhysicalMedium" ;;

let terms_PhysicalResource = terms_"PhysicalResource" ;;

let terms_Point = terms_"Point" ;;

let terms_Policy = terms_"Policy" ;;

let terms_provenance = terms_"provenance" ;;

let terms_ProvenanceStatement = terms_"ProvenanceStatement" ;;

let terms_publisher = terms_"publisher" ;;

let terms_references = terms_"references" ;;

let terms_relation = terms_"relation" ;;

let terms_replaces = terms_"replaces" ;;

let terms_requires = terms_"requires" ;;

let terms_RFC1766 = terms_"RFC1766" ;;

let terms_RFC3066 = terms_"RFC3066" ;;

let terms_RFC4646 = terms_"RFC4646" ;;

let terms_RFC5646 = terms_"RFC5646" ;;

let terms_rights = terms_"rights" ;;

let terms_rightsHolder = terms_"rightsHolder" ;;

let terms_RightsStatement = terms_"RightsStatement" ;;

let terms_SizeOrDuration = terms_"SizeOrDuration" ;;

let terms_source = terms_"source" ;;

let terms_spatial = terms_"spatial" ;;

let terms_Standard = terms_"Standard" ;;

let terms_subject = terms_"subject" ;;

let terms_tableOfContents = terms_"tableOfContents" ;;

let terms_temporal = terms_"temporal" ;;

let terms_title = terms_"title" ;;

let terms_type = terms_"type" ;;

let terms_URI = terms_"URI" ;;

let terms_valid = terms_"valid" ;;

let terms_W3CDTF = terms_"W3CDTF" ;;


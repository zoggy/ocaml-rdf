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

(** Elements of [http://usefulinc.com/ns/doap#] *)

(** [http://usefulinc.com/ns/doap#] *)
val doap : Rdf_iri.iri
val doap_ : string -> Rdf_iri.iri

(** Repository for anonymous access. *)
val doap_anon_root : Rdf_iri.iri

(** GNU Arch source code repository. *)
val doap_ArchRepository : Rdf_iri.iri

(** Description of target user base *)
val doap_audience : Rdf_iri.iri

(** Bazaar source code branch. *)
val doap_BazaarBranch : Rdf_iri.iri

(** BitKeeper source code repository. *)
val doap_BKRepository : Rdf_iri.iri

(** URI of a blog related to a project *)
val doap_blog : Rdf_iri.iri

(** Web browser interface to repository. *)
val doap_browse : Rdf_iri.iri

(** Bug tracker for a project. *)
val doap_bug_database : Rdf_iri.iri

(** A category of project. *)
val doap_category : Rdf_iri.iri

(** Date when something was created, in YYYY-MM-DD form. e.g. 2004-04-05 *)
val doap_created : Rdf_iri.iri

(** CVS source code repository. *)
val doap_CVSRepository : Rdf_iri.iri

(** darcs source code repository. *)
val doap_DarcsRepository : Rdf_iri.iri

(** Plain text description of a project, of 2-4 sentences in length. *)
val doap_description : Rdf_iri.iri

(** Developer of software for the project. *)
val doap_developer : Rdf_iri.iri

(** Contributor of documentation to the project. *)
val doap_documenter : Rdf_iri.iri

(** Mirror of software download web page. *)
val doap_download_mirror : Rdf_iri.iri

(** Web page from which the project software can be downloaded. *)
val doap_download_page : Rdf_iri.iri

(** URI of download associated with this release. *)
val doap_file_release : Rdf_iri.iri

(** Git source code repository. *)
val doap_GitRepository : Rdf_iri.iri

(** Project contributor. *)
val doap_helper : Rdf_iri.iri

(** Mercurial source code repository. *)
val doap_HgRepository : Rdf_iri.iri

(** URL of a project's homepage, associated with exactly one project. *)
val doap_homepage : Rdf_iri.iri

(** A specification that a project implements. Could be a standard, API or legally defined level of conformance. *)
val doap_implements : Rdf_iri.iri

(** ISO language code a project has been translated into *)
val doap_language : Rdf_iri.iri

(** The URI of an RDF description of the license the software is distributed under. *)
val doap_license : Rdf_iri.iri

(** Location of a repository. *)
val doap_location : Rdf_iri.iri

(** Mailing list home page or email address. *)
val doap_mailing_list : Rdf_iri.iri

(** Maintainer of a project, a project leader. *)
val doap_maintainer : Rdf_iri.iri

(** Module name of a repository. *)
val doap_module : Rdf_iri.iri

(** A name of something. *)
val doap_name : Rdf_iri.iri

(** URL of a project's past homepage, associated with exactly one project. *)
val doap_old_homepage : Rdf_iri.iri

(** Operating system that a project is limited to. Omit this property if the project is not OS-specific. *)
val doap_os : Rdf_iri.iri

(** Indicator of software platform (non-OS specific), e.g. Java, Firefox, ECMA CLR *)
val doap_platform : Rdf_iri.iri

(** Programming language a project is implemented in or intended for use with. *)
val doap_programming_language : Rdf_iri.iri

(** A project. *)
val doap_Project : Rdf_iri.iri

(** A project release. *)
val doap_release : Rdf_iri.iri

(** Source code repository. *)
val doap_Repository : Rdf_iri.iri

(** Source code repository. *)
val doap_repository : Rdf_iri.iri

(** Revision identifier of a software release. *)
val doap_revision : Rdf_iri.iri

(** Web page with screenshots of project. *)
val doap_screenshots : Rdf_iri.iri

(** The URI of a web service endpoint where software as a service may be accessed *)
val doap_service_endpoint : Rdf_iri.iri

(** Short (8 or 9 words) plain text description of a project. *)
val doap_shortdesc : Rdf_iri.iri

(** A specification of a system's aspects, technical or otherwise. *)
val doap_Specification : Rdf_iri.iri

(** Subversion source code repository. *)
val doap_SVNRepository : Rdf_iri.iri

(** A tester or other quality control contributor. *)
val doap_tester : Rdf_iri.iri

(** Contributor of translations to the project. *)
val doap_translator : Rdf_iri.iri

(** Vendor organization: commercial, free or otherwise *)
val doap_vendor : Rdf_iri.iri

(** Version information of a project release. *)
val doap_Version : Rdf_iri.iri

(** URL of Wiki for collaborative discussion of project. *)
val doap_wiki : Rdf_iri.iri


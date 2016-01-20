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

(** Elements of [http://usefulinc.com/ns/doap#] *)

(** [http://usefulinc.com/ns/doap#] *)
val doap : Iri.iri
val doap_ : string -> Iri.iri

(** Repository for anonymous access. *)
val doap_anon_root : Iri.iri

(** GNU Arch source code repository. *)
val doap_ArchRepository : Iri.iri

(** Description of target user base *)
val doap_audience : Iri.iri

(** Bazaar source code branch. *)
val doap_BazaarBranch : Iri.iri

(** BitKeeper source code repository. *)
val doap_BKRepository : Iri.iri

(** URI of a blog related to a project *)
val doap_blog : Iri.iri

(** Web browser interface to repository. *)
val doap_browse : Iri.iri

(** Bug tracker for a project. *)
val doap_bug_database : Iri.iri

(** A category of project. *)
val doap_category : Iri.iri

(** Date when something was created, in YYYY-MM-DD form. e.g. 2004-04-05 *)
val doap_created : Iri.iri

(** CVS source code repository. *)
val doap_CVSRepository : Iri.iri

(** darcs source code repository. *)
val doap_DarcsRepository : Iri.iri

(** Plain text description of a project, of 2-4 sentences in length. *)
val doap_description : Iri.iri

(** Developer of software for the project. *)
val doap_developer : Iri.iri

(** Contributor of documentation to the project. *)
val doap_documenter : Iri.iri

(** Mirror of software download web page. *)
val doap_download_mirror : Iri.iri

(** Web page from which the project software can be downloaded. *)
val doap_download_page : Iri.iri

(** URI of download associated with this release. *)
val doap_file_release : Iri.iri

(** Git source code repository. *)
val doap_GitRepository : Iri.iri

(** Project contributor. *)
val doap_helper : Iri.iri

(** Mercurial source code repository. *)
val doap_HgRepository : Iri.iri

(** URL of a project's homepage, associated with exactly one project. *)
val doap_homepage : Iri.iri

(** A specification that a project implements. Could be a standard, API or legally defined level of conformance. *)
val doap_implements : Iri.iri

(** ISO language code a project has been translated into *)
val doap_language : Iri.iri

(** The URI of an RDF description of the license the software is distributed under. *)
val doap_license : Iri.iri

(** Location of a repository. *)
val doap_location : Iri.iri

(** Mailing list home page or email address. *)
val doap_mailing_list : Iri.iri

(** Maintainer of a project, a project leader. *)
val doap_maintainer : Iri.iri

(** Module name of a repository. *)
val doap_module : Iri.iri

(** A name of something. *)
val doap_name : Iri.iri

(** URL of a project's past homepage, associated with exactly one project. *)
val doap_old_homepage : Iri.iri

(** Operating system that a project is limited to. Omit this property if the project is not OS-specific. *)
val doap_os : Iri.iri

(** Indicator of software platform (non-OS specific), e.g. Java, Firefox, ECMA CLR *)
val doap_platform : Iri.iri

(** Programming language a project is implemented in or intended for use with. *)
val doap_programming_language : Iri.iri

(** A project. *)
val doap_Project : Iri.iri

(** A project release. *)
val doap_release : Iri.iri

(** Source code repository. *)
val doap_Repository : Iri.iri

(** Source code repository. *)
val doap_repository : Iri.iri

(** Revision identifier of a software release. *)
val doap_revision : Iri.iri

(** Web page with screenshots of project. *)
val doap_screenshots : Iri.iri

(** The URI of a web service endpoint where software as a service may be accessed *)
val doap_service_endpoint : Iri.iri

(** Short (8 or 9 words) plain text description of a project. *)
val doap_shortdesc : Iri.iri

(** A specification of a system's aspects, technical or otherwise. *)
val doap_Specification : Iri.iri

(** Subversion source code repository. *)
val doap_SVNRepository : Iri.iri

(** A tester or other quality control contributor. *)
val doap_tester : Iri.iri

(** Contributor of translations to the project. *)
val doap_translator : Iri.iri

(** Vendor organization: commercial, free or otherwise *)
val doap_vendor : Iri.iri

(** Version information of a project release. *)
val doap_Version : Iri.iri

(** URL of Wiki for collaborative discussion of project. *)
val doap_wiki : Iri.iri


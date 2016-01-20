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
val doap : Iri.t
val doap_ : string -> Iri.t

(** Repository for anonymous access. *)
val doap_anon_root : Iri.t

(** GNU Arch source code repository. *)
val doap_ArchRepository : Iri.t

(** Description of target user base *)
val doap_audience : Iri.t

(** Bazaar source code branch. *)
val doap_BazaarBranch : Iri.t

(** BitKeeper source code repository. *)
val doap_BKRepository : Iri.t

(** URI of a blog related to a project *)
val doap_blog : Iri.t

(** Web browser interface to repository. *)
val doap_browse : Iri.t

(** Bug tracker for a project. *)
val doap_bug_database : Iri.t

(** A category of project. *)
val doap_category : Iri.t

(** Date when something was created, in YYYY-MM-DD form. e.g. 2004-04-05 *)
val doap_created : Iri.t

(** CVS source code repository. *)
val doap_CVSRepository : Iri.t

(** darcs source code repository. *)
val doap_DarcsRepository : Iri.t

(** Plain text description of a project, of 2-4 sentences in length. *)
val doap_description : Iri.t

(** Developer of software for the project. *)
val doap_developer : Iri.t

(** Contributor of documentation to the project. *)
val doap_documenter : Iri.t

(** Mirror of software download web page. *)
val doap_download_mirror : Iri.t

(** Web page from which the project software can be downloaded. *)
val doap_download_page : Iri.t

(** URI of download associated with this release. *)
val doap_file_release : Iri.t

(** Git source code repository. *)
val doap_GitRepository : Iri.t

(** Project contributor. *)
val doap_helper : Iri.t

(** Mercurial source code repository. *)
val doap_HgRepository : Iri.t

(** URL of a project's homepage, associated with exactly one project. *)
val doap_homepage : Iri.t

(** A specification that a project implements. Could be a standard, API or legally defined level of conformance. *)
val doap_implements : Iri.t

(** ISO language code a project has been translated into *)
val doap_language : Iri.t

(** The URI of an RDF description of the license the software is distributed under. *)
val doap_license : Iri.t

(** Location of a repository. *)
val doap_location : Iri.t

(** Mailing list home page or email address. *)
val doap_mailing_list : Iri.t

(** Maintainer of a project, a project leader. *)
val doap_maintainer : Iri.t

(** Module name of a repository. *)
val doap_module : Iri.t

(** A name of something. *)
val doap_name : Iri.t

(** URL of a project's past homepage, associated with exactly one project. *)
val doap_old_homepage : Iri.t

(** Operating system that a project is limited to. Omit this property if the project is not OS-specific. *)
val doap_os : Iri.t

(** Indicator of software platform (non-OS specific), e.g. Java, Firefox, ECMA CLR *)
val doap_platform : Iri.t

(** Programming language a project is implemented in or intended for use with. *)
val doap_programming_language : Iri.t

(** A project. *)
val doap_Project : Iri.t

(** A project release. *)
val doap_release : Iri.t

(** Source code repository. *)
val doap_Repository : Iri.t

(** Source code repository. *)
val doap_repository : Iri.t

(** Revision identifier of a software release. *)
val doap_revision : Iri.t

(** Web page with screenshots of project. *)
val doap_screenshots : Iri.t

(** The URI of a web service endpoint where software as a service may be accessed *)
val doap_service_endpoint : Iri.t

(** Short (8 or 9 words) plain text description of a project. *)
val doap_shortdesc : Iri.t

(** A specification of a system's aspects, technical or otherwise. *)
val doap_Specification : Iri.t

(** Subversion source code repository. *)
val doap_SVNRepository : Iri.t

(** A tester or other quality control contributor. *)
val doap_tester : Iri.t

(** Contributor of translations to the project. *)
val doap_translator : Iri.t

(** Vendor organization: commercial, free or otherwise *)
val doap_vendor : Iri.t

(** Version information of a project release. *)
val doap_Version : Iri.t

(** URL of Wiki for collaborative discussion of project. *)
val doap_wiki : Iri.t


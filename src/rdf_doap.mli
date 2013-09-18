(** Elements of [http://usefulinc.com/ns/doap#] *)

(** [http://usefulinc.com/ns/doap#] *)
val doap : Rdf_uri.uri
val doap_ : string -> Rdf_uri.uri

(** Repository for anonymous access. *)
val doap_anon_root : Rdf_uri.uri

(** GNU Arch source code repository. *)
val doap_ArchRepository : Rdf_uri.uri

(** Description of target user base *)
val doap_audience : Rdf_uri.uri

(** Bazaar source code branch. *)
val doap_BazaarBranch : Rdf_uri.uri

(** BitKeeper source code repository. *)
val doap_BKRepository : Rdf_uri.uri

(** URI of a blog related to a project *)
val doap_blog : Rdf_uri.uri

(** Web browser interface to repository. *)
val doap_browse : Rdf_uri.uri

(** Bug tracker for a project. *)
val doap_bug_database : Rdf_uri.uri

(** A category of project. *)
val doap_category : Rdf_uri.uri

(** Date when something was created, in YYYY-MM-DD form. e.g. 2004-04-05 *)
val doap_created : Rdf_uri.uri

(** CVS source code repository. *)
val doap_CVSRepository : Rdf_uri.uri

(** darcs source code repository. *)
val doap_DarcsRepository : Rdf_uri.uri

(** Plain text description of a project, of 2-4 sentences in length. *)
val doap_description : Rdf_uri.uri

(** Developer of software for the project. *)
val doap_developer : Rdf_uri.uri

(** Contributor of documentation to the project. *)
val doap_documenter : Rdf_uri.uri

(** Mirror of software download web page. *)
val doap_download_mirror : Rdf_uri.uri

(** Web page from which the project software can be downloaded. *)
val doap_download_page : Rdf_uri.uri

(** URI of download associated with this release. *)
val doap_file_release : Rdf_uri.uri

(** Git source code repository. *)
val doap_GitRepository : Rdf_uri.uri

(** Project contributor. *)
val doap_helper : Rdf_uri.uri

(** Mercurial source code repository. *)
val doap_HgRepository : Rdf_uri.uri

(** URL of a project's homepage, associated with exactly one project. *)
val doap_homepage : Rdf_uri.uri

(** A specification that a project implements. Could be a standard, API or legally defined level of conformance. *)
val doap_implements : Rdf_uri.uri

(** ISO language code a project has been translated into *)
val doap_language : Rdf_uri.uri

(** The URI of an RDF description of the license the software is distributed under. *)
val doap_license : Rdf_uri.uri

(** Location of a repository. *)
val doap_location : Rdf_uri.uri

(** Mailing list home page or email address. *)
val doap_mailing_list : Rdf_uri.uri

(** Maintainer of a project, a project leader. *)
val doap_maintainer : Rdf_uri.uri

(** Module name of a repository. *)
val doap_module : Rdf_uri.uri

(** A name of something. *)
val doap_name : Rdf_uri.uri

(** URL of a project's past homepage, associated with exactly one project. *)
val doap_old_homepage : Rdf_uri.uri

(** Operating system that a project is limited to. Omit this property if the project is not OS-specific. *)
val doap_os : Rdf_uri.uri

(** Indicator of software platform (non-OS specific), e.g. Java, Firefox, ECMA CLR *)
val doap_platform : Rdf_uri.uri

(** Programming language a project is implemented in or intended for use with. *)
val doap_programming_language : Rdf_uri.uri

(** A project. *)
val doap_Project : Rdf_uri.uri

(** A project release. *)
val doap_release : Rdf_uri.uri

(** Source code repository. *)
val doap_Repository : Rdf_uri.uri

(** Source code repository. *)
val doap_repository : Rdf_uri.uri

(** Revision identifier of a software release. *)
val doap_revision : Rdf_uri.uri

(** Web page with screenshots of project. *)
val doap_screenshots : Rdf_uri.uri

(** The URI of a web service endpoint where software as a service may be accessed *)
val doap_service_endpoint : Rdf_uri.uri

(** Short (8 or 9 words) plain text description of a project. *)
val doap_shortdesc : Rdf_uri.uri

(** A specification of a system's aspects, technical or otherwise. *)
val doap_Specification : Rdf_uri.uri

(** Subversion source code repository. *)
val doap_SVNRepository : Rdf_uri.uri

(** A tester or other quality control contributor. *)
val doap_tester : Rdf_uri.uri

(** Contributor of translations to the project. *)
val doap_translator : Rdf_uri.uri

(** Vendor organization: commercial, free or otherwise *)
val doap_vendor : Rdf_uri.uri

(** Version information of a project release. *)
val doap_Version : Rdf_uri.uri

(** URL of Wiki for collaborative discussion of project. *)
val doap_wiki : Rdf_uri.uri


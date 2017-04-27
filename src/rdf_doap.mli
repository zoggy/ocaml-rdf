(** Elements of [http://usefulinc.com/ns/doap#] *)

(** [http://usefulinc.com/ns/doap#] *)
val doap : Iri.t
val doap_ : string -> Iri.t

(** GNU Arch source code repository. *)
val c_ArchRepository : Iri.t

(** BitKeeper source code repository. *)
val c_BKRepository : Iri.t

(** Bazaar source code branch. *)
val c_BazaarBranch : Iri.t

(** CVS source code repository. *)
val c_CVSRepository : Iri.t

(** darcs source code repository. *)
val c_DarcsRepository : Iri.t

(** Git source code repository. *)
val c_GitRepository : Iri.t

(** Mercurial source code repository. *)
val c_HgRepository : Iri.t

(** A project. *)
val c_Project : Iri.t

(** Source code repository. *)
val c_Repository : Iri.t

(** Subversion source code repository. *)
val c_SVNRepository : Iri.t

(** A specification of a system's aspects, technical or otherwise. *)
val c_Specification : Iri.t

(** Version information of a project release. *)
val c_Version : Iri.t

(** Repository for anonymous access. *)
val anon_root : Iri.t

(** Description of target user base *)
val audience : Iri.t

(** URI of a blog related to a project *)
val blog : Iri.t

(** Web browser interface to repository. *)
val browse : Iri.t

(** Bug tracker for a project. *)
val bug_database : Iri.t

(** A category of project. *)
val category : Iri.t

(** Date when something was created, in YYYY-MM-DD form. e.g. 2004-04-05 *)
val created : Iri.t

(** Plain text description of a project, of 2-4 sentences in length. *)
val description : Iri.t

(** Developer of software for the project. *)
val developer : Iri.t

(** Contributor of documentation to the project. *)
val documenter : Iri.t

(** Mirror of software download web page. *)
val download_mirror : Iri.t

(** Web page from which the project software can be downloaded. *)
val download_page : Iri.t

(** URI of download associated with this release. *)
val file_release : Iri.t

(** Project contributor. *)
val helper : Iri.t

(** URL of a project's homepage, associated with exactly one project. *)
val homepage : Iri.t

(** A specification that a project implements. Could be a standard, API or legally defined level of conformance. *)
val implements : Iri.t

(** このプロジェクトの翻訳された言語のISO言語コート *)
val language : Iri.t

(** The URI of an RDF description of the license the software is distributed under. *)
val license : Iri.t

(** Location of a repository. *)
val location : Iri.t

(** Mailing list home page or email address. *)
val mailing_list : Iri.t

(** Maintainer of a project, a project leader. *)
val maintainer : Iri.t

(** Module name of a repository. *)
val module_ : Iri.t

(** A name of something. *)
val name : Iri.t

(** URL of a project's past homepage, associated with exactly one project. *)
val old_homepage : Iri.t

(** Operating system that a project is limited to. Omit this property if the project is not OS-specific. *)
val os : Iri.t

(** Indicator of software platform (non-OS specific), e.g. Java, Firefox, ECMA CLR *)
val platform : Iri.t

(** Programming language a project is implemented in or intended for use with. *)
val programming_language : Iri.t

(** A project release. *)
val release : Iri.t

(** Source code repository. *)
val repository : Iri.t

(** Revision identifier of a software release. *)
val revision : Iri.t

(** Web page with screenshots of project. *)
val screenshots : Iri.t

(** The URI of a web service endpoint where software as a service may be accessed *)
val service_endpoint : Iri.t

(** Short (8 or 9 words) plain text description of a project. *)
val shortdesc : Iri.t

(** このプロジェクトのため、試験をする人や品質メンテナー。 *)
val tester : Iri.t

(** Contributor of translations to the project. *)
val translator : Iri.t

(** Vendor organization: commercial, free or otherwise *)
val vendor : Iri.t

(** URL of Wiki for collaborative discussion of project. *)
val wiki : Iri.t


module Open : sig
  (** GNU Arch source code repository. *)
  val doap_c_ArchRepository : Iri.t

  (** BitKeeper source code repository. *)
  val doap_c_BKRepository : Iri.t

  (** Bazaar source code branch. *)
  val doap_c_BazaarBranch : Iri.t

  (** CVS source code repository. *)
  val doap_c_CVSRepository : Iri.t

  (** darcs source code repository. *)
  val doap_c_DarcsRepository : Iri.t

  (** Git source code repository. *)
  val doap_c_GitRepository : Iri.t

  (** Mercurial source code repository. *)
  val doap_c_HgRepository : Iri.t

  (** A project. *)
  val doap_c_Project : Iri.t

  (** Source code repository. *)
  val doap_c_Repository : Iri.t

  (** Subversion source code repository. *)
  val doap_c_SVNRepository : Iri.t

  (** A specification of a system's aspects, technical or otherwise. *)
  val doap_c_Specification : Iri.t

  (** Version information of a project release. *)
  val doap_c_Version : Iri.t

  (** Repository for anonymous access. *)
  val doap_anon_root : Iri.t

  (** Description of target user base *)
  val doap_audience : Iri.t

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

  (** Project contributor. *)
  val doap_helper : Iri.t

  (** URL of a project's homepage, associated with exactly one project. *)
  val doap_homepage : Iri.t

  (** A specification that a project implements. Could be a standard, API or legally defined level of conformance. *)
  val doap_implements : Iri.t

  (** このプロジェクトの翻訳された言語のISO言語コート *)
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

  (** A project release. *)
  val doap_release : Iri.t

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

  (** このプロジェクトのため、試験をする人や品質メンテナー。 *)
  val doap_tester : Iri.t

  (** Contributor of translations to the project. *)
  val doap_translator : Iri.t

  (** Vendor organization: commercial, free or otherwise *)
  val doap_vendor : Iri.t

  (** URL of Wiki for collaborative discussion of project. *)
  val doap_wiki : Iri.t

end

class from : ?sub: Iri.t -> Rdf_graph.graph ->
  object
    method anon_root : Rdf_term.literal list
    method anon_root_opt : Rdf_term.literal option
    method audience : Rdf_term.literal list
    method audience_opt : Rdf_term.literal option
    method blog : Iri.t list
    method blog_opt : Iri.t option
    method browse : Iri.t list
    method browse_opt : Iri.t option
    method bug_database : Iri.t list
    method bug_database_opt : Iri.t option
    method category : Iri.t list
    method category_opt : Iri.t option
    method created : Rdf_term.literal list
    method created_opt : Rdf_term.literal option
    method description : Rdf_term.literal list
    method description_opt : Rdf_term.literal option
    method developer : Iri.t list
    method developer_opt : Iri.t option
    method documenter : Iri.t list
    method documenter_opt : Iri.t option
    method download_mirror : Iri.t list
    method download_mirror_opt : Iri.t option
    method download_page : Iri.t list
    method download_page_opt : Iri.t option
    method file_release : Iri.t list
    method file_release_opt : Iri.t option
    method helper : Iri.t list
    method helper_opt : Iri.t option
    method homepage : Iri.t list
    method homepage_opt : Iri.t option
    method implements : Iri.t list
    method implements_opt : Iri.t option
    method language : Rdf_term.literal list
    method language_opt : Rdf_term.literal option
    method license : Iri.t list
    method license_opt : Iri.t option
    method location : Iri.t list
    method location_opt : Iri.t option
    method mailing_list : Iri.t list
    method mailing_list_opt : Iri.t option
    method maintainer : Iri.t list
    method maintainer_opt : Iri.t option
    method module_ : Iri.t list
    method module__opt : Iri.t option
    method name : Rdf_term.literal list
    method name_opt : Rdf_term.literal option
    method old_homepage : Iri.t list
    method old_homepage_opt : Iri.t option
    method os : Rdf_term.literal list
    method os_opt : Rdf_term.literal option
    method platform : Rdf_term.literal list
    method platform_opt : Rdf_term.literal option
    method programming_language : Rdf_term.literal list
    method programming_language_opt : Rdf_term.literal option
    method release : Iri.t list
    method release_opt : Iri.t option
    method repository : Iri.t list
    method repository_opt : Iri.t option
    method revision : Rdf_term.literal list
    method revision_opt : Rdf_term.literal option
    method screenshots : Iri.t list
    method screenshots_opt : Iri.t option
    method service_endpoint : Iri.t list
    method service_endpoint_opt : Iri.t option
    method shortdesc : Rdf_term.literal list
    method shortdesc_opt : Rdf_term.literal option
    method tester : Iri.t list
    method tester_opt : Iri.t option
    method translator : Iri.t list
    method translator_opt : Iri.t option
    method vendor : Iri.t list
    method vendor_opt : Iri.t option
    method wiki : Iri.t list
    method wiki_opt : Iri.t option
  end

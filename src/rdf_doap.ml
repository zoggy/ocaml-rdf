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

let doap = Rdf_iri.iri "http://usefulinc.com/ns/doap#";;
let doap_ = Rdf_iri.append doap;;

let doap_anon_root = doap_"anon-root" ;;

let doap_ArchRepository = doap_"ArchRepository" ;;

let doap_audience = doap_"audience" ;;

let doap_BazaarBranch = doap_"BazaarBranch" ;;

let doap_BKRepository = doap_"BKRepository" ;;

let doap_blog = doap_"blog" ;;

let doap_browse = doap_"browse" ;;

let doap_bug_database = doap_"bug-database" ;;

let doap_category = doap_"category" ;;

let doap_created = doap_"created" ;;

let doap_CVSRepository = doap_"CVSRepository" ;;

let doap_DarcsRepository = doap_"DarcsRepository" ;;

let doap_description = doap_"description" ;;

let doap_developer = doap_"developer" ;;

let doap_documenter = doap_"documenter" ;;

let doap_download_mirror = doap_"download-mirror" ;;

let doap_download_page = doap_"download-page" ;;

let doap_file_release = doap_"file-release" ;;

let doap_GitRepository = doap_"GitRepository" ;;

let doap_helper = doap_"helper" ;;

let doap_HgRepository = doap_"HgRepository" ;;

let doap_homepage = doap_"homepage" ;;

let doap_implements = doap_"implements" ;;

let doap_language = doap_"language" ;;

let doap_license = doap_"license" ;;

let doap_location = doap_"location" ;;

let doap_mailing_list = doap_"mailing-list" ;;

let doap_maintainer = doap_"maintainer" ;;

let doap_module = doap_"module" ;;

let doap_name = doap_"name" ;;

let doap_old_homepage = doap_"old-homepage" ;;

let doap_os = doap_"os" ;;

let doap_platform = doap_"platform" ;;

let doap_programming_language = doap_"programming-language" ;;

let doap_Project = doap_"Project" ;;

let doap_release = doap_"release" ;;

let doap_Repository = doap_"Repository" ;;

let doap_repository = doap_"repository" ;;

let doap_revision = doap_"revision" ;;

let doap_screenshots = doap_"screenshots" ;;

let doap_service_endpoint = doap_"service-endpoint" ;;

let doap_shortdesc = doap_"shortdesc" ;;

let doap_Specification = doap_"Specification" ;;

let doap_SVNRepository = doap_"SVNRepository" ;;

let doap_tester = doap_"tester" ;;

let doap_translator = doap_"translator" ;;

let doap_vendor = doap_"vendor" ;;

let doap_Version = doap_"Version" ;;

let doap_wiki = doap_"wiki" ;;


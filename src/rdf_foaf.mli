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

(** Elements of [http://xmlns.com/foaf/0.1/] *)

(** [http://xmlns.com/foaf/0.1/] *)
val foaf : Rdf_iri.iri
val foaf_ : string -> Rdf_iri.iri

(** Indicates an account held by this agent. *)
val foaf_account : Rdf_iri.iri

(** Indicates the name (identifier) associated with this online account. *)
val foaf_accountName : Rdf_iri.iri

(** Indicates a homepage of the service provide for this online account. *)
val foaf_accountServiceHomepage : Rdf_iri.iri

(** The age in years of some agent. *)
val foaf_age : Rdf_iri.iri

(** An agent (eg. person, group, software or physical artifact). *)
val foaf_Agent : Rdf_iri.iri

(** An AIM chat ID *)
val foaf_aimChatID : Rdf_iri.iri

(** A location that something is based near, for some broadly human notion of near. *)
val foaf_based_near : Rdf_iri.iri

(** The birthday of this Agent, represented in mm-dd string form, eg. '12-31'. *)
val foaf_birthday : Rdf_iri.iri

(** A current project this person works on. *)
val foaf_currentProject : Rdf_iri.iri

(** A depiction of some thing. *)
val foaf_depiction : Rdf_iri.iri

(** A thing depicted in this representation. *)
val foaf_depicts : Rdf_iri.iri

(** A checksum for the DNA of some thing. Joke. *)
val foaf_dnaChecksum : Rdf_iri.iri

(** A document. *)
val foaf_Document : Rdf_iri.iri

(** The family name of some person. *)
val foaf_family_name : Rdf_iri.iri

(** The family name of some person. *)
val foaf_familyName : Rdf_iri.iri

(** The first name of a person. *)
val foaf_firstName : Rdf_iri.iri

(** The underlying or 'focal' entity associated with some SKOS-described concept. *)
val foaf_focus : Rdf_iri.iri

(** An organization funding a project or person. *)
val foaf_fundedBy : Rdf_iri.iri

(** A textual geekcode for this person, see http://www.geekcode.com/geek.html *)
val foaf_geekcode : Rdf_iri.iri

(** The gender of this Agent (typically but not necessarily 'male' or 'female'). *)
val foaf_gender : Rdf_iri.iri

(** The given name of some person. *)
val foaf_givenname : Rdf_iri.iri

(** The given name of some person. *)
val foaf_givenName : Rdf_iri.iri

(** A class of Agents. *)
val foaf_Group : Rdf_iri.iri

(** Indicates an account held by this agent. *)
val foaf_holdsAccount : Rdf_iri.iri

(** A homepage for some thing. *)
val foaf_homepage : Rdf_iri.iri

(** An ICQ chat ID *)
val foaf_icqChatID : Rdf_iri.iri

(** An image. *)
val foaf_Image : Rdf_iri.iri

(** An image that can be used to represent some thing (ie. those depictions which are particularly representative of something, eg. one's photo on a homepage). *)
val foaf_img : Rdf_iri.iri

(** A page about a topic of interest to this person. *)
val foaf_interest : Rdf_iri.iri

(** A document that this thing is the primary topic of. *)
val foaf_isPrimaryTopicOf : Rdf_iri.iri

(** A jabber ID for something. *)
val foaf_jabberID : Rdf_iri.iri

(** A person known by this person (indicating some level of reciprocated interaction between the parties). *)
val foaf_knows : Rdf_iri.iri

(** A foaf:LabelProperty is any RDF property with texual values that serve as labels. *)
val foaf_LabelProperty : Rdf_iri.iri

(** The last name of a person. *)
val foaf_lastName : Rdf_iri.iri

(** A logo representing some thing. *)
val foaf_logo : Rdf_iri.iri

(** Something that was made by this agent. *)
val foaf_made : Rdf_iri.iri

(** An agent that made this thing. *)
val foaf_maker : Rdf_iri.iri

(** A personal mailbox, ie. an Internet mailbox associated with exactly one owner, the first owner of this mailbox. This is a 'static inverse functional property', in that there is (across time and change) at most one individual that ever has any particular value for foaf:mbox. *)
val foaf_mbox : Rdf_iri.iri

(** The sha1sum of the URI of an Internet mailbox associated with exactly one owner, the first owner of the mailbox. *)
val foaf_mbox_sha1sum : Rdf_iri.iri

(** Indicates a member of a Group *)
val foaf_member : Rdf_iri.iri

(** Indicates the class of individuals that are a member of a Group *)
val foaf_membershipClass : Rdf_iri.iri

(** An MSN chat ID *)
val foaf_msnChatID : Rdf_iri.iri

(** A Myers Briggs (MBTI) personality classification. *)
val foaf_myersBriggs : Rdf_iri.iri

(** A name for some thing. *)
val foaf_name : Rdf_iri.iri

(** A short informal nickname characterising an agent (includes login identifiers, IRC and other chat nicknames). *)
val foaf_nick : Rdf_iri.iri

(** An online account. *)
val foaf_OnlineAccount : Rdf_iri.iri

(** An online chat account. *)
val foaf_OnlineChatAccount : Rdf_iri.iri

(** An online e-commerce account. *)
val foaf_OnlineEcommerceAccount : Rdf_iri.iri

(** An online gaming account. *)
val foaf_OnlineGamingAccount : Rdf_iri.iri

(** An OpenID for an Agent. *)
val foaf_openid : Rdf_iri.iri

(** An organization. *)
val foaf_Organization : Rdf_iri.iri

(** A page or document about this thing. *)
val foaf_page : Rdf_iri.iri

(** A project this person has previously worked on. *)
val foaf_pastProject : Rdf_iri.iri

(** A person. *)
val foaf_Person : Rdf_iri.iri

(** A personal profile RDF document. *)
val foaf_PersonalProfileDocument : Rdf_iri.iri

(** A phone, specified using fully qualified tel: URI scheme (refs: http://www.w3.org/Addressing/schemes.html#tel). *)
val foaf_phone : Rdf_iri.iri

(** A .plan comment, in the tradition of finger and '.plan' files. *)
val foaf_plan : Rdf_iri.iri

(** The primary topic of some page or document. *)
val foaf_primaryTopic : Rdf_iri.iri

(** A project (a collective endeavour of some kind). *)
val foaf_Project : Rdf_iri.iri

(** A link to the publications of this person. *)
val foaf_publications : Rdf_iri.iri

(** A homepage of a school attended by the person. *)
val foaf_schoolHomepage : Rdf_iri.iri

(** A sha1sum hash, in hex. *)
val foaf_sha1 : Rdf_iri.iri

(** A Skype ID *)
val foaf_skypeID : Rdf_iri.iri

(** A string expressing what the user is happy for the general public (normally) to know about their current activity. *)
val foaf_status : Rdf_iri.iri

(** The surname of some person. *)
val foaf_surname : Rdf_iri.iri

(** A theme. *)
val foaf_theme : Rdf_iri.iri

(** A derived thumbnail image. *)
val foaf_thumbnail : Rdf_iri.iri

(** A tipjar document for this agent, describing means for payment and reward. *)
val foaf_tipjar : Rdf_iri.iri

(** Title (Mr, Mrs, Ms, Dr. etc) *)
val foaf_title : Rdf_iri.iri

(** A topic of some page or document. *)
val foaf_topic : Rdf_iri.iri

(** A thing of interest to this person. *)
val foaf_topic_interest : Rdf_iri.iri

(** A weblog of some thing (whether person, group, company etc.). *)
val foaf_weblog : Rdf_iri.iri

(** A work info homepage of some person; a page about their work for some organization. *)
val foaf_workInfoHomepage : Rdf_iri.iri

(** A workplace homepage of some person; the homepage of an organization they work for. *)
val foaf_workplaceHomepage : Rdf_iri.iri

(** A Yahoo chat ID *)
val foaf_yahooChatID : Rdf_iri.iri


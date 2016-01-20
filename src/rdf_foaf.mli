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
val foaf : Iri.iri
val foaf_ : string -> Iri.iri

(** Indicates an account held by this agent. *)
val foaf_account : Iri.iri

(** Indicates the name (identifier) associated with this online account. *)
val foaf_accountName : Iri.iri

(** Indicates a homepage of the service provide for this online account. *)
val foaf_accountServiceHomepage : Iri.iri

(** The age in years of some agent. *)
val foaf_age : Iri.iri

(** An agent (eg. person, group, software or physical artifact). *)
val foaf_Agent : Iri.iri

(** An AIM chat ID *)
val foaf_aimChatID : Iri.iri

(** A location that something is based near, for some broadly human notion of near. *)
val foaf_based_near : Iri.iri

(** The birthday of this Agent, represented in mm-dd string form, eg. '12-31'. *)
val foaf_birthday : Iri.iri

(** A current project this person works on. *)
val foaf_currentProject : Iri.iri

(** A depiction of some thing. *)
val foaf_depiction : Iri.iri

(** A thing depicted in this representation. *)
val foaf_depicts : Iri.iri

(** A checksum for the DNA of some thing. Joke. *)
val foaf_dnaChecksum : Iri.iri

(** A document. *)
val foaf_Document : Iri.iri

(** The family name of some person. *)
val foaf_family_name : Iri.iri

(** The family name of some person. *)
val foaf_familyName : Iri.iri

(** The first name of a person. *)
val foaf_firstName : Iri.iri

(** The underlying or 'focal' entity associated with some SKOS-described concept. *)
val foaf_focus : Iri.iri

(** An organization funding a project or person. *)
val foaf_fundedBy : Iri.iri

(** A textual geekcode for this person, see http://www.geekcode.com/geek.html *)
val foaf_geekcode : Iri.iri

(** The gender of this Agent (typically but not necessarily 'male' or 'female'). *)
val foaf_gender : Iri.iri

(** The given name of some person. *)
val foaf_givenname : Iri.iri

(** The given name of some person. *)
val foaf_givenName : Iri.iri

(** A class of Agents. *)
val foaf_Group : Iri.iri

(** Indicates an account held by this agent. *)
val foaf_holdsAccount : Iri.iri

(** A homepage for some thing. *)
val foaf_homepage : Iri.iri

(** An ICQ chat ID *)
val foaf_icqChatID : Iri.iri

(** An image. *)
val foaf_Image : Iri.iri

(** An image that can be used to represent some thing (ie. those depictions which are particularly representative of something, eg. one's photo on a homepage). *)
val foaf_img : Iri.iri

(** A page about a topic of interest to this person. *)
val foaf_interest : Iri.iri

(** A document that this thing is the primary topic of. *)
val foaf_isPrimaryTopicOf : Iri.iri

(** A jabber ID for something. *)
val foaf_jabberID : Iri.iri

(** A person known by this person (indicating some level of reciprocated interaction between the parties). *)
val foaf_knows : Iri.iri

(** A foaf:LabelProperty is any RDF property with texual values that serve as labels. *)
val foaf_LabelProperty : Iri.iri

(** The last name of a person. *)
val foaf_lastName : Iri.iri

(** A logo representing some thing. *)
val foaf_logo : Iri.iri

(** Something that was made by this agent. *)
val foaf_made : Iri.iri

(** An agent that made this thing. *)
val foaf_maker : Iri.iri

(** A personal mailbox, ie. an Internet mailbox associated with exactly one owner, the first owner of this mailbox. This is a 'static inverse functional property', in that there is (across time and change) at most one individual that ever has any particular value for foaf:mbox. *)
val foaf_mbox : Iri.iri

(** The sha1sum of the URI of an Internet mailbox associated with exactly one owner, the first owner of the mailbox. *)
val foaf_mbox_sha1sum : Iri.iri

(** Indicates a member of a Group *)
val foaf_member : Iri.iri

(** Indicates the class of individuals that are a member of a Group *)
val foaf_membershipClass : Iri.iri

(** An MSN chat ID *)
val foaf_msnChatID : Iri.iri

(** A Myers Briggs (MBTI) personality classification. *)
val foaf_myersBriggs : Iri.iri

(** A name for some thing. *)
val foaf_name : Iri.iri

(** A short informal nickname characterising an agent (includes login identifiers, IRC and other chat nicknames). *)
val foaf_nick : Iri.iri

(** An online account. *)
val foaf_OnlineAccount : Iri.iri

(** An online chat account. *)
val foaf_OnlineChatAccount : Iri.iri

(** An online e-commerce account. *)
val foaf_OnlineEcommerceAccount : Iri.iri

(** An online gaming account. *)
val foaf_OnlineGamingAccount : Iri.iri

(** An OpenID for an Agent. *)
val foaf_openid : Iri.iri

(** An organization. *)
val foaf_Organization : Iri.iri

(** A page or document about this thing. *)
val foaf_page : Iri.iri

(** A project this person has previously worked on. *)
val foaf_pastProject : Iri.iri

(** A person. *)
val foaf_Person : Iri.iri

(** A personal profile RDF document. *)
val foaf_PersonalProfileDocument : Iri.iri

(** A phone, specified using fully qualified tel: URI scheme (refs: http://www.w3.org/Addressing/schemes.html#tel). *)
val foaf_phone : Iri.iri

(** A .plan comment, in the tradition of finger and '.plan' files. *)
val foaf_plan : Iri.iri

(** The primary topic of some page or document. *)
val foaf_primaryTopic : Iri.iri

(** A project (a collective endeavour of some kind). *)
val foaf_Project : Iri.iri

(** A link to the publications of this person. *)
val foaf_publications : Iri.iri

(** A homepage of a school attended by the person. *)
val foaf_schoolHomepage : Iri.iri

(** A sha1sum hash, in hex. *)
val foaf_sha1 : Iri.iri

(** A Skype ID *)
val foaf_skypeID : Iri.iri

(** A string expressing what the user is happy for the general public (normally) to know about their current activity. *)
val foaf_status : Iri.iri

(** The surname of some person. *)
val foaf_surname : Iri.iri

(** A theme. *)
val foaf_theme : Iri.iri

(** A derived thumbnail image. *)
val foaf_thumbnail : Iri.iri

(** A tipjar document for this agent, describing means for payment and reward. *)
val foaf_tipjar : Iri.iri

(** Title (Mr, Mrs, Ms, Dr. etc) *)
val foaf_title : Iri.iri

(** A topic of some page or document. *)
val foaf_topic : Iri.iri

(** A thing of interest to this person. *)
val foaf_topic_interest : Iri.iri

(** A weblog of some thing (whether person, group, company etc.). *)
val foaf_weblog : Iri.iri

(** A work info homepage of some person; a page about their work for some organization. *)
val foaf_workInfoHomepage : Iri.iri

(** A workplace homepage of some person; the homepage of an organization they work for. *)
val foaf_workplaceHomepage : Iri.iri

(** A Yahoo chat ID *)
val foaf_yahooChatID : Iri.iri


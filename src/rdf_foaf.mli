(*********************************************************************************)
(*                OCaml-RDF                                                      *)
(*                                                                               *)
(*    Copyright (C) 2012-2016 Institut National de Recherche en Informatique     *)
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
val foaf : Iri.t
val foaf_ : string -> Iri.t

(** Indicates an account held by this agent. *)
val foaf_account : Iri.t

(** Indicates the name (identifier) associated with this online account. *)
val foaf_accountName : Iri.t

(** Indicates a homepage of the service provide for this online account. *)
val foaf_accountServiceHomepage : Iri.t

(** The age in years of some agent. *)
val foaf_age : Iri.t

(** An agent (eg. person, group, software or physical artifact). *)
val foaf_Agent : Iri.t

(** An AIM chat ID *)
val foaf_aimChatID : Iri.t

(** A location that something is based near, for some broadly human notion of near. *)
val foaf_based_near : Iri.t

(** The birthday of this Agent, represented in mm-dd string form, eg. '12-31'. *)
val foaf_birthday : Iri.t

(** A current project this person works on. *)
val foaf_currentProject : Iri.t

(** A depiction of some thing. *)
val foaf_depiction : Iri.t

(** A thing depicted in this representation. *)
val foaf_depicts : Iri.t

(** A checksum for the DNA of some thing. Joke. *)
val foaf_dnaChecksum : Iri.t

(** A document. *)
val foaf_Document : Iri.t

(** The family name of some person. *)
val foaf_family_name : Iri.t

(** The family name of some person. *)
val foaf_familyName : Iri.t

(** The first name of a person. *)
val foaf_firstName : Iri.t

(** The underlying or 'focal' entity associated with some SKOS-described concept. *)
val foaf_focus : Iri.t

(** An organization funding a project or person. *)
val foaf_fundedBy : Iri.t

(** A textual geekcode for this person, see http://www.geekcode.com/geek.html *)
val foaf_geekcode : Iri.t

(** The gender of this Agent (typically but not necessarily 'male' or 'female'). *)
val foaf_gender : Iri.t

(** The given name of some person. *)
val foaf_givenname : Iri.t

(** The given name of some person. *)
val foaf_givenName : Iri.t

(** A class of Agents. *)
val foaf_Group : Iri.t

(** Indicates an account held by this agent. *)
val foaf_holdsAccount : Iri.t

(** A homepage for some thing. *)
val foaf_homepage : Iri.t

(** An ICQ chat ID *)
val foaf_icqChatID : Iri.t

(** An image. *)
val foaf_Image : Iri.t

(** An image that can be used to represent some thing (ie. those depictions which are particularly representative of something, eg. one's photo on a homepage). *)
val foaf_img : Iri.t

(** A page about a topic of interest to this person. *)
val foaf_interest : Iri.t

(** A document that this thing is the primary topic of. *)
val foaf_isPrimaryTopicOf : Iri.t

(** A jabber ID for something. *)
val foaf_jabberID : Iri.t

(** A person known by this person (indicating some level of reciprocated interaction between the parties). *)
val foaf_knows : Iri.t

(** A foaf:LabelProperty is any RDF property with texual values that serve as labels. *)
val foaf_LabelProperty : Iri.t

(** The last name of a person. *)
val foaf_lastName : Iri.t

(** A logo representing some thing. *)
val foaf_logo : Iri.t

(** Something that was made by this agent. *)
val foaf_made : Iri.t

(** An agent that made this thing. *)
val foaf_maker : Iri.t

(** A personal mailbox, ie. an Internet mailbox associated with exactly one owner, the first owner of this mailbox. This is a 'static inverse functional property', in that there is (across time and change) at most one individual that ever has any particular value for foaf:mbox. *)
val foaf_mbox : Iri.t

(** The sha1sum of the URI of an Internet mailbox associated with exactly one owner, the first owner of the mailbox. *)
val foaf_mbox_sha1sum : Iri.t

(** Indicates a member of a Group *)
val foaf_member : Iri.t

(** Indicates the class of individuals that are a member of a Group *)
val foaf_membershipClass : Iri.t

(** An MSN chat ID *)
val foaf_msnChatID : Iri.t

(** A Myers Briggs (MBTI) personality classification. *)
val foaf_myersBriggs : Iri.t

(** A name for some thing. *)
val foaf_name : Iri.t

(** A short informal nickname characterising an agent (includes login identifiers, IRC and other chat nicknames). *)
val foaf_nick : Iri.t

(** An online account. *)
val foaf_OnlineAccount : Iri.t

(** An online chat account. *)
val foaf_OnlineChatAccount : Iri.t

(** An online e-commerce account. *)
val foaf_OnlineEcommerceAccount : Iri.t

(** An online gaming account. *)
val foaf_OnlineGamingAccount : Iri.t

(** An OpenID for an Agent. *)
val foaf_openid : Iri.t

(** An organization. *)
val foaf_Organization : Iri.t

(** A page or document about this thing. *)
val foaf_page : Iri.t

(** A project this person has previously worked on. *)
val foaf_pastProject : Iri.t

(** A person. *)
val foaf_Person : Iri.t

(** A personal profile RDF document. *)
val foaf_PersonalProfileDocument : Iri.t

(** A phone, specified using fully qualified tel: URI scheme (refs: http://www.w3.org/Addressing/schemes.html#tel). *)
val foaf_phone : Iri.t

(** A .plan comment, in the tradition of finger and '.plan' files. *)
val foaf_plan : Iri.t

(** The primary topic of some page or document. *)
val foaf_primaryTopic : Iri.t

(** A project (a collective endeavour of some kind). *)
val foaf_Project : Iri.t

(** A link to the publications of this person. *)
val foaf_publications : Iri.t

(** A homepage of a school attended by the person. *)
val foaf_schoolHomepage : Iri.t

(** A sha1sum hash, in hex. *)
val foaf_sha1 : Iri.t

(** A Skype ID *)
val foaf_skypeID : Iri.t

(** A string expressing what the user is happy for the general public (normally) to know about their current activity. *)
val foaf_status : Iri.t

(** The surname of some person. *)
val foaf_surname : Iri.t

(** A theme. *)
val foaf_theme : Iri.t

(** A derived thumbnail image. *)
val foaf_thumbnail : Iri.t

(** A tipjar document for this agent, describing means for payment and reward. *)
val foaf_tipjar : Iri.t

(** Title (Mr, Mrs, Ms, Dr. etc) *)
val foaf_title : Iri.t

(** A topic of some page or document. *)
val foaf_topic : Iri.t

(** A thing of interest to this person. *)
val foaf_topic_interest : Iri.t

(** A weblog of some thing (whether person, group, company etc.). *)
val foaf_weblog : Iri.t

(** A work info homepage of some person; a page about their work for some organization. *)
val foaf_workInfoHomepage : Iri.t

(** A workplace homepage of some person; the homepage of an organization they work for. *)
val foaf_workplaceHomepage : Iri.t

(** A Yahoo chat ID *)
val foaf_yahooChatID : Iri.t


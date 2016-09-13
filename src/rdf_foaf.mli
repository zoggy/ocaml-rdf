(** Elements of [http://xmlns.com/foaf/0.1/] *)

(** [http://xmlns.com/foaf/0.1/] *)
val foaf : Iri.t
val foaf_ : string -> Iri.t

(** An agent (eg. person, group, software or physical artifact). *)
val c_Agent : Iri.t

(** A document. *)
val c_Document : Iri.t

(** A class of Agents. *)
val c_Group : Iri.t

(** An image. *)
val c_Image : Iri.t

(** A foaf:LabelProperty is any RDF property with texual values that serve as labels. *)
val c_LabelProperty : Iri.t

(** An online account. *)
val c_OnlineAccount : Iri.t

(** An online chat account. *)
val c_OnlineChatAccount : Iri.t

(** An online e-commerce account. *)
val c_OnlineEcommerceAccount : Iri.t

(** An online gaming account. *)
val c_OnlineGamingAccount : Iri.t

(** An organization. *)
val c_Organization : Iri.t

(** A person. *)
val c_Person : Iri.t

(** A personal profile RDF document. *)
val c_PersonalProfileDocument : Iri.t

(** A project (a collective endeavour of some kind). *)
val c_Project : Iri.t

(** Indicates an account held by this agent. *)
val account : Iri.t

(** Indicates the name (identifier) associated with this online account. *)
val accountName : Iri.t

(** Indicates a homepage of the service provide for this online account. *)
val accountServiceHomepage : Iri.t

(** The age in years of some agent. *)
val age : Iri.t

(** An AIM chat ID *)
val aimChatID : Iri.t

(** A location that something is based near, for some broadly human notion of near. *)
val based_near : Iri.t

(** The birthday of this Agent, represented in mm-dd string form, eg. '12-31'. *)
val birthday : Iri.t

(** A current project this person works on. *)
val currentProject : Iri.t

(** A depiction of some thing. *)
val depiction : Iri.t

(** A thing depicted in this representation. *)
val depicts : Iri.t

(** A checksum for the DNA of some thing. Joke. *)
val dnaChecksum : Iri.t

(** The family name of some person. *)
val familyName : Iri.t

(** The family name of some person. *)
val family_name : Iri.t

(** The first name of a person. *)
val firstName : Iri.t

(** The underlying or 'focal' entity associated with some SKOS-described concept. *)
val focus : Iri.t

(** An organization funding a project or person. *)
val fundedBy : Iri.t

(** A textual geekcode for this person, see http://www.geekcode.com/geek.html *)
val geekcode : Iri.t

(** The gender of this Agent (typically but not necessarily 'male' or 'female'). *)
val gender : Iri.t

(** The given name of some person. *)
val givenName : Iri.t

(** The given name of some person. *)
val givenname : Iri.t

(** Indicates an account held by this agent. *)
val holdsAccount : Iri.t

(** A homepage for some thing. *)
val homepage : Iri.t

(** An ICQ chat ID *)
val icqChatID : Iri.t

(** An image that can be used to represent some thing (ie. those depictions which are particularly representative of something, eg. one's photo on a homepage). *)
val img : Iri.t

(** A page about a topic of interest to this person. *)
val interest : Iri.t

(** A document that this thing is the primary topic of. *)
val isPrimaryTopicOf : Iri.t

(** A jabber ID for something. *)
val jabberID : Iri.t

(** A person known by this person (indicating some level of reciprocated interaction between the parties). *)
val knows : Iri.t

(** The last name of a person. *)
val lastName : Iri.t

(** A logo representing some thing. *)
val logo : Iri.t

(** Something that was made by this agent. *)
val made : Iri.t

(** An agent that made this thing. *)
val maker : Iri.t

(** A personal mailbox, ie. an Internet mailbox associated with exactly one owner, the first owner of this mailbox. This is a 'static inverse functional property', in that there is (across time and change) at most one individual that ever has any particular value for foaf:mbox. *)
val mbox : Iri.t

(** The sha1sum of the URI of an Internet mailbox associated with exactly one owner, the first owner of the mailbox. *)
val mbox_sha1sum : Iri.t

(** Indicates a member of a Group *)
val member : Iri.t

(** Indicates the class of individuals that are a member of a Group *)
val membershipClass : Iri.t

(** An MSN chat ID *)
val msnChatID : Iri.t

(** A Myers Briggs (MBTI) personality classification. *)
val myersBriggs : Iri.t

(** A name for some thing. *)
val name : Iri.t

(** A short informal nickname characterising an agent (includes login identifiers, IRC and other chat nicknames). *)
val nick : Iri.t

(** An OpenID for an Agent. *)
val openid : Iri.t

(** A page or document about this thing. *)
val page : Iri.t

(** A project this person has previously worked on. *)
val pastProject : Iri.t

(** A phone, specified using fully qualified tel: URI scheme (refs: http://www.w3.org/Addressing/schemes.html#tel). *)
val phone : Iri.t

(** A .plan comment, in the tradition of finger and '.plan' files. *)
val plan : Iri.t

(** The primary topic of some page or document. *)
val primaryTopic : Iri.t

(** A link to the publications of this person. *)
val publications : Iri.t

(** A homepage of a school attended by the person. *)
val schoolHomepage : Iri.t

(** A sha1sum hash, in hex. *)
val sha1 : Iri.t

(** A Skype ID *)
val skypeID : Iri.t

(** A string expressing what the user is happy for the general public (normally) to know about their current activity. *)
val status : Iri.t

(** The surname of some person. *)
val surname : Iri.t

(** A theme. *)
val theme : Iri.t

(** A derived thumbnail image. *)
val thumbnail : Iri.t

(** A tipjar document for this agent, describing means for payment and reward. *)
val tipjar : Iri.t

(** Title (Mr, Mrs, Ms, Dr. etc) *)
val title : Iri.t

(** A topic of some page or document. *)
val topic : Iri.t

(** A thing of interest to this person. *)
val topic_interest : Iri.t

(** A weblog of some thing (whether person, group, company etc.). *)
val weblog : Iri.t

(** A work info homepage of some person; a page about their work for some organization. *)
val workInfoHomepage : Iri.t

(** A workplace homepage of some person; the homepage of an organization they work for. *)
val workplaceHomepage : Iri.t

(** A Yahoo chat ID *)
val yahooChatID : Iri.t


module Open : sig
  (** An agent (eg. person, group, software or physical artifact). *)
  val foaf_c_Agent : Iri.t

  (** A document. *)
  val foaf_c_Document : Iri.t

  (** A class of Agents. *)
  val foaf_c_Group : Iri.t

  (** An image. *)
  val foaf_c_Image : Iri.t

  (** A foaf:LabelProperty is any RDF property with texual values that serve as labels. *)
  val foaf_c_LabelProperty : Iri.t

  (** An online account. *)
  val foaf_c_OnlineAccount : Iri.t

  (** An online chat account. *)
  val foaf_c_OnlineChatAccount : Iri.t

  (** An online e-commerce account. *)
  val foaf_c_OnlineEcommerceAccount : Iri.t

  (** An online gaming account. *)
  val foaf_c_OnlineGamingAccount : Iri.t

  (** An organization. *)
  val foaf_c_Organization : Iri.t

  (** A person. *)
  val foaf_c_Person : Iri.t

  (** A personal profile RDF document. *)
  val foaf_c_PersonalProfileDocument : Iri.t

  (** A project (a collective endeavour of some kind). *)
  val foaf_c_Project : Iri.t

  (** Indicates an account held by this agent. *)
  val foaf_account : Iri.t

  (** Indicates the name (identifier) associated with this online account. *)
  val foaf_accountName : Iri.t

  (** Indicates a homepage of the service provide for this online account. *)
  val foaf_accountServiceHomepage : Iri.t

  (** The age in years of some agent. *)
  val foaf_age : Iri.t

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

  (** The family name of some person. *)
  val foaf_familyName : Iri.t

  (** The family name of some person. *)
  val foaf_family_name : Iri.t

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
  val foaf_givenName : Iri.t

  (** The given name of some person. *)
  val foaf_givenname : Iri.t

  (** Indicates an account held by this agent. *)
  val foaf_holdsAccount : Iri.t

  (** A homepage for some thing. *)
  val foaf_homepage : Iri.t

  (** An ICQ chat ID *)
  val foaf_icqChatID : Iri.t

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

  (** An OpenID for an Agent. *)
  val foaf_openid : Iri.t

  (** A page or document about this thing. *)
  val foaf_page : Iri.t

  (** A project this person has previously worked on. *)
  val foaf_pastProject : Iri.t

  (** A phone, specified using fully qualified tel: URI scheme (refs: http://www.w3.org/Addressing/schemes.html#tel). *)
  val foaf_phone : Iri.t

  (** A .plan comment, in the tradition of finger and '.plan' files. *)
  val foaf_plan : Iri.t

  (** The primary topic of some page or document. *)
  val foaf_primaryTopic : Iri.t

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

end

class from : ?sub: Iri.t -> Rdf_graph.graph ->
  object
    method account : Iri.t list
    method accountName : Rdf_term.literal list
    method accountServiceHomepage : Iri.t list
    method age : Rdf_term.literal list
    method aimChatID : Rdf_term.literal list
    method based_near : Iri.t list
    method birthday : Rdf_term.literal list
    method currentProject : Iri.t list
    method depiction : Iri.t list
    method depicts : Iri.t list
    method dnaChecksum : Rdf_term.literal list
    method familyName : Rdf_term.literal list
    method family_name : Rdf_term.literal list
    method firstName : Rdf_term.literal list
    method focus : Iri.t list
    method fundedBy : Iri.t list
    method geekcode : Rdf_term.literal list
    method gender : Rdf_term.literal list
    method givenName : Iri.t list
    method givenname : Iri.t list
    method holdsAccount : Iri.t list
    method homepage : Iri.t list
    method icqChatID : Rdf_term.literal list
    method img : Iri.t list
    method interest : Iri.t list
    method isPrimaryTopicOf : Iri.t list
    method jabberID : Rdf_term.literal list
    method knows : Iri.t list
    method lastName : Rdf_term.literal list
    method logo : Iri.t list
    method made : Iri.t list
    method maker : Iri.t list
    method mbox : Iri.t list
    method mbox_sha1sum : Rdf_term.literal list
    method member : Iri.t list
    method membershipClass : Iri.t list
    method msnChatID : Rdf_term.literal list
    method myersBriggs : Rdf_term.literal list
    method name : Rdf_term.literal list
    method nick : Iri.t list
    method openid : Iri.t list
    method page : Iri.t list
    method pastProject : Iri.t list
    method phone : Iri.t list
    method plan : Rdf_term.literal list
    method primaryTopic : Iri.t list
    method publications : Iri.t list
    method schoolHomepage : Iri.t list
    method sha1 : Iri.t list
    method skypeID : Rdf_term.literal list
    method status : Rdf_term.literal list
    method surname : Rdf_term.literal list
    method theme : Iri.t list
    method thumbnail : Iri.t list
    method tipjar : Iri.t list
    method title : Iri.t list
    method topic : Iri.t list
    method topic_interest : Iri.t list
    method weblog : Iri.t list
    method workInfoHomepage : Iri.t list
    method workplaceHomepage : Iri.t list
    method yahooChatID : Rdf_term.literal list
  end

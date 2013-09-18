(** Elements of [http://xmlns.com/foaf/0.1/] *)

(** [http://xmlns.com/foaf/0.1/] *)
val foaf : Rdf_uri.uri
val foaf_ : string -> Rdf_uri.uri

(** Indicates an account held by this agent. *)
val foaf_account : Rdf_uri.uri

(** Indicates the name (identifier) associated with this online account. *)
val foaf_accountName : Rdf_uri.uri

(** Indicates a homepage of the service provide for this online account. *)
val foaf_accountServiceHomepage : Rdf_uri.uri

(** The age in years of some agent. *)
val foaf_age : Rdf_uri.uri

(** An agent (eg. person, group, software or physical artifact). *)
val foaf_Agent : Rdf_uri.uri

(** An AIM chat ID *)
val foaf_aimChatID : Rdf_uri.uri

(** A location that something is based near, for some broadly human notion of near. *)
val foaf_based_near : Rdf_uri.uri

(** The birthday of this Agent, represented in mm-dd string form, eg. '12-31'. *)
val foaf_birthday : Rdf_uri.uri

(** A current project this person works on. *)
val foaf_currentProject : Rdf_uri.uri

(** A depiction of some thing. *)
val foaf_depiction : Rdf_uri.uri

(** A thing depicted in this representation. *)
val foaf_depicts : Rdf_uri.uri

(** A checksum for the DNA of some thing. Joke. *)
val foaf_dnaChecksum : Rdf_uri.uri

(** A document. *)
val foaf_Document : Rdf_uri.uri

(** The family name of some person. *)
val foaf_family_name : Rdf_uri.uri

(** The family name of some person. *)
val foaf_familyName : Rdf_uri.uri

(** The first name of a person. *)
val foaf_firstName : Rdf_uri.uri

(** The underlying or 'focal' entity associated with some SKOS-described concept. *)
val foaf_focus : Rdf_uri.uri

(** An organization funding a project or person. *)
val foaf_fundedBy : Rdf_uri.uri

(** A textual geekcode for this person, see http://www.geekcode.com/geek.html *)
val foaf_geekcode : Rdf_uri.uri

(** The gender of this Agent (typically but not necessarily 'male' or 'female'). *)
val foaf_gender : Rdf_uri.uri

(** The given name of some person. *)
val foaf_givenname : Rdf_uri.uri

(** The given name of some person. *)
val foaf_givenName : Rdf_uri.uri

(** A class of Agents. *)
val foaf_Group : Rdf_uri.uri

(** Indicates an account held by this agent. *)
val foaf_holdsAccount : Rdf_uri.uri

(** A homepage for some thing. *)
val foaf_homepage : Rdf_uri.uri

(** An ICQ chat ID *)
val foaf_icqChatID : Rdf_uri.uri

(** An image. *)
val foaf_Image : Rdf_uri.uri

(** An image that can be used to represent some thing (ie. those depictions which are particularly representative of something, eg. one's photo on a homepage). *)
val foaf_img : Rdf_uri.uri

(** A page about a topic of interest to this person. *)
val foaf_interest : Rdf_uri.uri

(** A document that this thing is the primary topic of. *)
val foaf_isPrimaryTopicOf : Rdf_uri.uri

(** A jabber ID for something. *)
val foaf_jabberID : Rdf_uri.uri

(** A person known by this person (indicating some level of reciprocated interaction between the parties). *)
val foaf_knows : Rdf_uri.uri

(** A foaf:LabelProperty is any RDF property with texual values that serve as labels. *)
val foaf_LabelProperty : Rdf_uri.uri

(** The last name of a person. *)
val foaf_lastName : Rdf_uri.uri

(** A logo representing some thing. *)
val foaf_logo : Rdf_uri.uri

(** Something that was made by this agent. *)
val foaf_made : Rdf_uri.uri

(** An agent that made this thing. *)
val foaf_maker : Rdf_uri.uri

(** A personal mailbox, ie. an Internet mailbox associated with exactly one owner, the first owner of this mailbox. This is a 'static inverse functional property', in that there is (across time and change) at most one individual that ever has any particular value for foaf:mbox. *)
val foaf_mbox : Rdf_uri.uri

(** The sha1sum of the URI of an Internet mailbox associated with exactly one owner, the first owner of the mailbox. *)
val foaf_mbox_sha1sum : Rdf_uri.uri

(** Indicates a member of a Group *)
val foaf_member : Rdf_uri.uri

(** Indicates the class of individuals that are a member of a Group *)
val foaf_membershipClass : Rdf_uri.uri

(** An MSN chat ID *)
val foaf_msnChatID : Rdf_uri.uri

(** A Myers Briggs (MBTI) personality classification. *)
val foaf_myersBriggs : Rdf_uri.uri

(** A name for some thing. *)
val foaf_name : Rdf_uri.uri

(** A short informal nickname characterising an agent (includes login identifiers, IRC and other chat nicknames). *)
val foaf_nick : Rdf_uri.uri

(** An online account. *)
val foaf_OnlineAccount : Rdf_uri.uri

(** An online chat account. *)
val foaf_OnlineChatAccount : Rdf_uri.uri

(** An online e-commerce account. *)
val foaf_OnlineEcommerceAccount : Rdf_uri.uri

(** An online gaming account. *)
val foaf_OnlineGamingAccount : Rdf_uri.uri

(** An OpenID for an Agent. *)
val foaf_openid : Rdf_uri.uri

(** An organization. *)
val foaf_Organization : Rdf_uri.uri

(** A page or document about this thing. *)
val foaf_page : Rdf_uri.uri

(** A project this person has previously worked on. *)
val foaf_pastProject : Rdf_uri.uri

(** A person. *)
val foaf_Person : Rdf_uri.uri

(** A personal profile RDF document. *)
val foaf_PersonalProfileDocument : Rdf_uri.uri

(** A phone, specified using fully qualified tel: URI scheme (refs: http://www.w3.org/Addressing/schemes.html#tel). *)
val foaf_phone : Rdf_uri.uri

(** A .plan comment, in the tradition of finger and '.plan' files. *)
val foaf_plan : Rdf_uri.uri

(** The primary topic of some page or document. *)
val foaf_primaryTopic : Rdf_uri.uri

(** A project (a collective endeavour of some kind). *)
val foaf_Project : Rdf_uri.uri

(** A link to the publications of this person. *)
val foaf_publications : Rdf_uri.uri

(** A homepage of a school attended by the person. *)
val foaf_schoolHomepage : Rdf_uri.uri

(** A sha1sum hash, in hex. *)
val foaf_sha1 : Rdf_uri.uri

(** A Skype ID *)
val foaf_skypeID : Rdf_uri.uri

(** A string expressing what the user is happy for the general public (normally) to know about their current activity. *)
val foaf_status : Rdf_uri.uri

(** The surname of some person. *)
val foaf_surname : Rdf_uri.uri

(** A theme. *)
val foaf_theme : Rdf_uri.uri

(** A derived thumbnail image. *)
val foaf_thumbnail : Rdf_uri.uri

(** A tipjar document for this agent, describing means for payment and reward. *)
val foaf_tipjar : Rdf_uri.uri

(** Title (Mr, Mrs, Ms, Dr. etc) *)
val foaf_title : Rdf_uri.uri

(** A topic of some page or document. *)
val foaf_topic : Rdf_uri.uri

(** A thing of interest to this person. *)
val foaf_topic_interest : Rdf_uri.uri

(** A weblog of some thing (whether person, group, company etc.). *)
val foaf_weblog : Rdf_uri.uri

(** A work info homepage of some person; a page about their work for some organization. *)
val foaf_workInfoHomepage : Rdf_uri.uri

(** A workplace homepage of some person; the homepage of an organization they work for. *)
val foaf_workplaceHomepage : Rdf_uri.uri

(** A Yahoo chat ID *)
val foaf_yahooChatID : Rdf_uri.uri


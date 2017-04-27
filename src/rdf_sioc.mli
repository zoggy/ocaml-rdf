(** Elements of [http://rdfs.org/sioc/ns#] *)

(** [http://rdfs.org/sioc/ns#] *)
val sioc : Iri.t
val sioc_ : string -> Iri.t

(** Community is a high-level concept that defines an online community and what it consists of. *)
val c_Community : Iri.t

(** An area in which content Items are contained. *)
val c_Container : Iri.t

(** A discussion area on which Posts or entries are made. *)
val c_Forum : Iri.t

(** An Item is something which can be in a Container. *)
val c_Item : Iri.t

(** An article or message that can be posted to a Forum. *)
val c_Post : Iri.t

(** A Role is a function of a UserAccount within a scope of a particular Forum, Site, etc. *)
val c_Role : Iri.t

(** A Site can be the location of an online community or set of communities, with UserAccounts and Usergroups creating Items in a set of Containers. It can be thought of as a web-accessible data Space. *)
val c_Site : Iri.t

(** A Space is a place where data resides, e.g. on a website, desktop, fileshare, etc. *)
val c_Space : Iri.t

(** A container for a series of threaded discussion Posts or Items. *)
val c_Thread : Iri.t

(** UserAccount is now preferred. This is a deprecated class for a User in an online community site. *)
val c_User : Iri.t

(** A user account in an online community site. *)
val c_UserAccount : Iri.t

(** A set of UserAccounts whose owners have a common purpose or interest. Can be used for access control purposes. *)
val c_Usergroup : Iri.t

(** Specifies that this Item is about a particular resource, e.g. a Post describing a book, hotel, etc. *)
val about : Iri.t

(** Refers to the foaf:Agent or foaf:Person who owns this sioc:UserAccount. *)
val account_of : Iri.t

(** Refers to who (e.g. a UserAccount, e-mail address, etc.) a particular Item is addressed to. *)
val addressed_to : Iri.t

(** A Site that the UserAccount is an administrator of. *)
val administrator_of : Iri.t

(** The URI of a file attached to an Item. *)
val attachment : Iri.t

(** An image or depiction used to represent this UserAccount. *)
val avatar : Iri.t

(** An Item that this Container contains. *)
val container_of : Iri.t

(** The content of the Item in plain text format. *)
val content : Iri.t

(** The encoded content of the Post, contained in CDATA areas. *)
val content_encoded : Iri.t

(** When this was created, in ISO 8601 format. *)
val created_at : Iri.t

(** A resource that the UserAccount is a creator of. *)
val creator_of : Iri.t

(** The content of the Post. *)
val description : Iri.t

(** An electronic mail address of the UserAccount. *)
val email : Iri.t

(** An electronic mail address of the UserAccount, encoded using SHA1. *)
val email_sha1 : Iri.t

(** This links Items to embedded statements, facts and structured content. *)
val embeds_knowledge : Iri.t

(** A feed (e.g. RSS, Atom, etc.) pertaining to this resource (e.g. for a Forum, Site, UserAccount, etc.). *)
val feed : Iri.t

(** First (real) name of this User. Synonyms include given name or christian name. *)
val first_name : Iri.t

(** Indicates that one UserAccount follows another UserAccount (e.g. for microblog posts or other content item updates). *)
val follows : Iri.t

(** A UserAccount that has this Role. *)
val function_of : Iri.t

val group_of : Iri.t

(** A UserAccount that is an administrator of this Site. *)
val has_administrator : Iri.t

(** The Container to which this Item belongs. *)
val has_container : Iri.t

(** This is the UserAccount that made this resource. *)
val has_creator : Iri.t

(** The discussion that is related to this Item. *)
val has_discussion : Iri.t

(** A Role that this UserAccount has. *)
val has_function : Iri.t

val has_group : Iri.t

(** The Site that hosts this Forum. *)
val has_host : Iri.t

(** A UserAccount that is a member of this Usergroup. *)
val has_member : Iri.t

(** A UserAccount that is a moderator of this Forum. *)
val has_moderator : Iri.t

(** A UserAccount that modified this Item. *)
val has_modifier : Iri.t

(** A UserAccount that this resource is owned by. *)
val has_owner : Iri.t

(** A Container or Forum that this Container or Forum is a child of. *)
val has_parent : Iri.t

(** An resource that is a part of this subject. *)
val has_part : Iri.t

(** Points to an Item or Post that is a reply or response to this Item or Post. *)
val has_reply : Iri.t

(** A resource that this Role applies to. *)
val has_scope : Iri.t

(** A data Space which this resource is a part of. *)
val has_space : Iri.t

(** A UserAccount that is subscribed to this Container. *)
val has_subscriber : Iri.t

(** Points to a Usergroup that has certain access to this Space. *)
val has_usergroup : Iri.t

(** A Forum that is hosted on this Site. *)
val host_of : Iri.t

(** An identifier of a SIOC concept instance. For example, a user ID. Must be unique for instances of each type of SIOC concept within the same site. *)
val id : Iri.t

(** The IP address used when creating this Item. This can be associated with a creator. Some wiki articles list the IP addresses for the creator or modifiers when the usernames are absent. *)
val ip_address : Iri.t

(** The date and time of the last activity associated with a SIOC concept instance, and expressed in ISO 8601 format. This could be due to a reply Post or Comment, a modification to an Item, etc. *)
val last_activity_date : Iri.t

(** The date and time of the last Post (or Item) in a Forum (or a Container), in ISO 8601 format. *)
val last_item_date : Iri.t

(** Last (real) name of this user. Synonyms include surname or family name. *)
val last_name : Iri.t

(** The date and time of the last reply Post or Comment, which could be associated with a starter Item or Post or with a Thread, and expressed in ISO 8601 format. *)
val last_reply_date : Iri.t

(** Links to the latest revision of this Item or Post. *)
val latest_version : Iri.t

(** A URI of a document which contains this SIOC object. *)
val link : Iri.t

(** Links extracted from hyperlinks within a SIOC concept, e.g. Post or Site. *)
val links_to : Iri.t

(** A Usergroup that this UserAccount is a member of. *)
val member_of : Iri.t

(** A Forum that a UserAccount is a moderator of. *)
val moderator_of : Iri.t

(** When this was modified, in ISO 8601 format. *)
val modified_at : Iri.t

(** An Item that this UserAccount has modified. *)
val modifier_of : Iri.t

(** The name of a SIOC concept instance, e.g. a username for a UserAccount, group name for a Usergroup, etc. *)
val name : Iri.t

(** Next Item or Post in a given Container sorted by date. *)
val next_by_date : Iri.t

(** Links to the next revision of this Item or Post. *)
val next_version : Iri.t

(** A note associated with this resource, for example, if it has been edited by a UserAccount. *)
val note : Iri.t

(** The number of unique authors (UserAccounts and unregistered posters) who have contributed to this Item, Thread, Post, etc. *)
val num_authors : Iri.t

(** The number of Posts (or Items) in a Forum (or a Container). *)
val num_items : Iri.t

(** The number of replies that this Item, Thread, Post, etc. has. Useful for when the reply structure is absent. *)
val num_replies : Iri.t

(** The number of Threads (AKA discussion topics) in a Forum. *)
val num_threads : Iri.t

(** The number of times this Item, Thread, UserAccount profile, etc. has been viewed. *)
val num_views : Iri.t

(** A resource owned by a particular UserAccount, for example, a weblog or image gallery. *)
val owner_of : Iri.t

(** A child Container or Forum that this Container or Forum is a parent of. *)
val parent_of : Iri.t

(** A resource that the subject is a part of. *)
val part_of : Iri.t

(** Previous Item or Post in a given Container sorted by date. *)
val previous_by_date : Iri.t

(** Links to the previous revision of this Item or Post. *)
val previous_version : Iri.t

(** Links either created explicitly or extracted implicitly on the HTML level from the Post. *)
val reference : Iri.t

(** Related Posts for this Post, perhaps determined implicitly from topics or references. *)
val related_to : Iri.t

(** Links to an Item or Post which this Item or Post is a reply to. *)
val reply_of : Iri.t

(** A Role that has a scope of this resource. *)
val scope_of : Iri.t

(** A resource which belongs to this data Space. *)
val space_of : Iri.t

(** Keyword(s) describing subject of the Post. *)
val subject : Iri.t

(** A Container that a UserAccount is subscribed to. *)
val subscriber_of : Iri.t

(** This is the title (subject line) of the Post. Note that for a Post within a threaded discussion that has no parents, it would detail the topic thread. *)
val title : Iri.t

(** A topic of interest, linking to the appropriate URI, e.g. in the Open Directory Project or of a SKOS category. *)
val topic : Iri.t

(** A Space that the Usergroup has access to. *)
val usergroup_of : Iri.t


module Open : sig
  (** Community is a high-level concept that defines an online community and what it consists of. *)
  val sioc_c_Community : Iri.t

  (** An area in which content Items are contained. *)
  val sioc_c_Container : Iri.t

  (** A discussion area on which Posts or entries are made. *)
  val sioc_c_Forum : Iri.t

  (** An Item is something which can be in a Container. *)
  val sioc_c_Item : Iri.t

  (** An article or message that can be posted to a Forum. *)
  val sioc_c_Post : Iri.t

  (** A Role is a function of a UserAccount within a scope of a particular Forum, Site, etc. *)
  val sioc_c_Role : Iri.t

  (** A Site can be the location of an online community or set of communities, with UserAccounts and Usergroups creating Items in a set of Containers. It can be thought of as a web-accessible data Space. *)
  val sioc_c_Site : Iri.t

  (** A Space is a place where data resides, e.g. on a website, desktop, fileshare, etc. *)
  val sioc_c_Space : Iri.t

  (** A container for a series of threaded discussion Posts or Items. *)
  val sioc_c_Thread : Iri.t

  (** UserAccount is now preferred. This is a deprecated class for a User in an online community site. *)
  val sioc_c_User : Iri.t

  (** A user account in an online community site. *)
  val sioc_c_UserAccount : Iri.t

  (** A set of UserAccounts whose owners have a common purpose or interest. Can be used for access control purposes. *)
  val sioc_c_Usergroup : Iri.t

  (** Specifies that this Item is about a particular resource, e.g. a Post describing a book, hotel, etc. *)
  val sioc_about : Iri.t

  (** Refers to the foaf:Agent or foaf:Person who owns this sioc:UserAccount. *)
  val sioc_account_of : Iri.t

  (** Refers to who (e.g. a UserAccount, e-mail address, etc.) a particular Item is addressed to. *)
  val sioc_addressed_to : Iri.t

  (** A Site that the UserAccount is an administrator of. *)
  val sioc_administrator_of : Iri.t

  (** The URI of a file attached to an Item. *)
  val sioc_attachment : Iri.t

  (** An image or depiction used to represent this UserAccount. *)
  val sioc_avatar : Iri.t

  (** An Item that this Container contains. *)
  val sioc_container_of : Iri.t

  (** The content of the Item in plain text format. *)
  val sioc_content : Iri.t

  (** The encoded content of the Post, contained in CDATA areas. *)
  val sioc_content_encoded : Iri.t

  (** When this was created, in ISO 8601 format. *)
  val sioc_created_at : Iri.t

  (** A resource that the UserAccount is a creator of. *)
  val sioc_creator_of : Iri.t

  (** The content of the Post. *)
  val sioc_description : Iri.t

  (** An electronic mail address of the UserAccount. *)
  val sioc_email : Iri.t

  (** An electronic mail address of the UserAccount, encoded using SHA1. *)
  val sioc_email_sha1 : Iri.t

  (** This links Items to embedded statements, facts and structured content. *)
  val sioc_embeds_knowledge : Iri.t

  (** A feed (e.g. RSS, Atom, etc.) pertaining to this resource (e.g. for a Forum, Site, UserAccount, etc.). *)
  val sioc_feed : Iri.t

  (** First (real) name of this User. Synonyms include given name or christian name. *)
  val sioc_first_name : Iri.t

  (** Indicates that one UserAccount follows another UserAccount (e.g. for microblog posts or other content item updates). *)
  val sioc_follows : Iri.t

  (** A UserAccount that has this Role. *)
  val sioc_function_of : Iri.t

  val sioc_group_of : Iri.t

  (** A UserAccount that is an administrator of this Site. *)
  val sioc_has_administrator : Iri.t

  (** The Container to which this Item belongs. *)
  val sioc_has_container : Iri.t

  (** This is the UserAccount that made this resource. *)
  val sioc_has_creator : Iri.t

  (** The discussion that is related to this Item. *)
  val sioc_has_discussion : Iri.t

  (** A Role that this UserAccount has. *)
  val sioc_has_function : Iri.t

  val sioc_has_group : Iri.t

  (** The Site that hosts this Forum. *)
  val sioc_has_host : Iri.t

  (** A UserAccount that is a member of this Usergroup. *)
  val sioc_has_member : Iri.t

  (** A UserAccount that is a moderator of this Forum. *)
  val sioc_has_moderator : Iri.t

  (** A UserAccount that modified this Item. *)
  val sioc_has_modifier : Iri.t

  (** A UserAccount that this resource is owned by. *)
  val sioc_has_owner : Iri.t

  (** A Container or Forum that this Container or Forum is a child of. *)
  val sioc_has_parent : Iri.t

  (** An resource that is a part of this subject. *)
  val sioc_has_part : Iri.t

  (** Points to an Item or Post that is a reply or response to this Item or Post. *)
  val sioc_has_reply : Iri.t

  (** A resource that this Role applies to. *)
  val sioc_has_scope : Iri.t

  (** A data Space which this resource is a part of. *)
  val sioc_has_space : Iri.t

  (** A UserAccount that is subscribed to this Container. *)
  val sioc_has_subscriber : Iri.t

  (** Points to a Usergroup that has certain access to this Space. *)
  val sioc_has_usergroup : Iri.t

  (** A Forum that is hosted on this Site. *)
  val sioc_host_of : Iri.t

  (** An identifier of a SIOC concept instance. For example, a user ID. Must be unique for instances of each type of SIOC concept within the same site. *)
  val sioc_id : Iri.t

  (** The IP address used when creating this Item. This can be associated with a creator. Some wiki articles list the IP addresses for the creator or modifiers when the usernames are absent. *)
  val sioc_ip_address : Iri.t

  (** The date and time of the last activity associated with a SIOC concept instance, and expressed in ISO 8601 format. This could be due to a reply Post or Comment, a modification to an Item, etc. *)
  val sioc_last_activity_date : Iri.t

  (** The date and time of the last Post (or Item) in a Forum (or a Container), in ISO 8601 format. *)
  val sioc_last_item_date : Iri.t

  (** Last (real) name of this user. Synonyms include surname or family name. *)
  val sioc_last_name : Iri.t

  (** The date and time of the last reply Post or Comment, which could be associated with a starter Item or Post or with a Thread, and expressed in ISO 8601 format. *)
  val sioc_last_reply_date : Iri.t

  (** Links to the latest revision of this Item or Post. *)
  val sioc_latest_version : Iri.t

  (** A URI of a document which contains this SIOC object. *)
  val sioc_link : Iri.t

  (** Links extracted from hyperlinks within a SIOC concept, e.g. Post or Site. *)
  val sioc_links_to : Iri.t

  (** A Usergroup that this UserAccount is a member of. *)
  val sioc_member_of : Iri.t

  (** A Forum that a UserAccount is a moderator of. *)
  val sioc_moderator_of : Iri.t

  (** When this was modified, in ISO 8601 format. *)
  val sioc_modified_at : Iri.t

  (** An Item that this UserAccount has modified. *)
  val sioc_modifier_of : Iri.t

  (** The name of a SIOC concept instance, e.g. a username for a UserAccount, group name for a Usergroup, etc. *)
  val sioc_name : Iri.t

  (** Next Item or Post in a given Container sorted by date. *)
  val sioc_next_by_date : Iri.t

  (** Links to the next revision of this Item or Post. *)
  val sioc_next_version : Iri.t

  (** A note associated with this resource, for example, if it has been edited by a UserAccount. *)
  val sioc_note : Iri.t

  (** The number of unique authors (UserAccounts and unregistered posters) who have contributed to this Item, Thread, Post, etc. *)
  val sioc_num_authors : Iri.t

  (** The number of Posts (or Items) in a Forum (or a Container). *)
  val sioc_num_items : Iri.t

  (** The number of replies that this Item, Thread, Post, etc. has. Useful for when the reply structure is absent. *)
  val sioc_num_replies : Iri.t

  (** The number of Threads (AKA discussion topics) in a Forum. *)
  val sioc_num_threads : Iri.t

  (** The number of times this Item, Thread, UserAccount profile, etc. has been viewed. *)
  val sioc_num_views : Iri.t

  (** A resource owned by a particular UserAccount, for example, a weblog or image gallery. *)
  val sioc_owner_of : Iri.t

  (** A child Container or Forum that this Container or Forum is a parent of. *)
  val sioc_parent_of : Iri.t

  (** A resource that the subject is a part of. *)
  val sioc_part_of : Iri.t

  (** Previous Item or Post in a given Container sorted by date. *)
  val sioc_previous_by_date : Iri.t

  (** Links to the previous revision of this Item or Post. *)
  val sioc_previous_version : Iri.t

  (** Links either created explicitly or extracted implicitly on the HTML level from the Post. *)
  val sioc_reference : Iri.t

  (** Related Posts for this Post, perhaps determined implicitly from topics or references. *)
  val sioc_related_to : Iri.t

  (** Links to an Item or Post which this Item or Post is a reply to. *)
  val sioc_reply_of : Iri.t

  (** A Role that has a scope of this resource. *)
  val sioc_scope_of : Iri.t

  (** A resource which belongs to this data Space. *)
  val sioc_space_of : Iri.t

  (** Keyword(s) describing subject of the Post. *)
  val sioc_subject : Iri.t

  (** A Container that a UserAccount is subscribed to. *)
  val sioc_subscriber_of : Iri.t

  (** This is the title (subject line) of the Post. Note that for a Post within a threaded discussion that has no parents, it would detail the topic thread. *)
  val sioc_title : Iri.t

  (** A topic of interest, linking to the appropriate URI, e.g. in the Open Directory Project or of a SKOS category. *)
  val sioc_topic : Iri.t

  (** A Space that the Usergroup has access to. *)
  val sioc_usergroup_of : Iri.t

end

class from : ?sub: Iri.t -> Rdf_graph.graph ->
  object
    method about : Iri.t list
    method about_opt : Iri.t option
    method account_of : Iri.t list
    method account_of_opt : Iri.t option
    method addressed_to : Iri.t list
    method addressed_to_opt : Iri.t option
    method administrator_of : Iri.t list
    method administrator_of_opt : Iri.t option
    method attachment : Iri.t list
    method attachment_opt : Iri.t option
    method avatar : Iri.t list
    method avatar_opt : Iri.t option
    method container_of : Iri.t list
    method container_of_opt : Iri.t option
    method content : Rdf_term.literal list
    method content_opt : Rdf_term.literal option
    method content_encoded : Rdf_term.literal list
    method content_encoded_opt : Rdf_term.literal option
    method created_at : Rdf_term.literal list
    method created_at_opt : Rdf_term.literal option
    method creator_of : Iri.t list
    method creator_of_opt : Iri.t option
    method description : Rdf_term.literal list
    method description_opt : Rdf_term.literal option
    method email : Iri.t list
    method email_opt : Iri.t option
    method email_sha1 : Rdf_term.literal list
    method email_sha1_opt : Rdf_term.literal option
    method embeds_knowledge : Iri.t list
    method embeds_knowledge_opt : Iri.t option
    method feed : Iri.t list
    method feed_opt : Iri.t option
    method first_name : Rdf_term.literal list
    method first_name_opt : Rdf_term.literal option
    method follows : Iri.t list
    method follows_opt : Iri.t option
    method function_of : Iri.t list
    method function_of_opt : Iri.t option
    method group_of : Iri.t list
    method group_of_opt : Iri.t option
    method has_administrator : Iri.t list
    method has_administrator_opt : Iri.t option
    method has_container : Iri.t list
    method has_container_opt : Iri.t option
    method has_creator : Iri.t list
    method has_creator_opt : Iri.t option
    method has_discussion : Iri.t list
    method has_discussion_opt : Iri.t option
    method has_function : Iri.t list
    method has_function_opt : Iri.t option
    method has_group : Iri.t list
    method has_group_opt : Iri.t option
    method has_host : Iri.t list
    method has_host_opt : Iri.t option
    method has_member : Iri.t list
    method has_member_opt : Iri.t option
    method has_moderator : Iri.t list
    method has_moderator_opt : Iri.t option
    method has_modifier : Iri.t list
    method has_modifier_opt : Iri.t option
    method has_owner : Iri.t list
    method has_owner_opt : Iri.t option
    method has_parent : Iri.t list
    method has_parent_opt : Iri.t option
    method has_part : Iri.t list
    method has_part_opt : Iri.t option
    method has_reply : Iri.t list
    method has_reply_opt : Iri.t option
    method has_scope : Iri.t list
    method has_scope_opt : Iri.t option
    method has_space : Iri.t list
    method has_space_opt : Iri.t option
    method has_subscriber : Iri.t list
    method has_subscriber_opt : Iri.t option
    method has_usergroup : Iri.t list
    method has_usergroup_opt : Iri.t option
    method host_of : Iri.t list
    method host_of_opt : Iri.t option
    method id : Rdf_term.literal list
    method id_opt : Rdf_term.literal option
    method ip_address : Rdf_term.literal list
    method ip_address_opt : Rdf_term.literal option
    method last_activity_date : Rdf_term.literal list
    method last_activity_date_opt : Rdf_term.literal option
    method last_item_date : Rdf_term.literal list
    method last_item_date_opt : Rdf_term.literal option
    method last_name : Rdf_term.literal list
    method last_name_opt : Rdf_term.literal option
    method last_reply_date : Rdf_term.literal list
    method last_reply_date_opt : Rdf_term.literal option
    method latest_version : Iri.t list
    method latest_version_opt : Iri.t option
    method link : Iri.t list
    method link_opt : Iri.t option
    method links_to : Iri.t list
    method links_to_opt : Iri.t option
    method member_of : Iri.t list
    method member_of_opt : Iri.t option
    method moderator_of : Iri.t list
    method moderator_of_opt : Iri.t option
    method modified_at : Rdf_term.literal list
    method modified_at_opt : Rdf_term.literal option
    method modifier_of : Iri.t list
    method modifier_of_opt : Iri.t option
    method name : Rdf_term.literal list
    method name_opt : Rdf_term.literal option
    method next_by_date : Iri.t list
    method next_by_date_opt : Iri.t option
    method next_version : Iri.t list
    method next_version_opt : Iri.t option
    method note : Rdf_term.literal list
    method note_opt : Rdf_term.literal option
    method num_authors : Rdf_term.literal list
    method num_authors_opt : Rdf_term.literal option
    method num_items : Rdf_term.literal list
    method num_items_opt : Rdf_term.literal option
    method num_replies : Rdf_term.literal list
    method num_replies_opt : Rdf_term.literal option
    method num_threads : Rdf_term.literal list
    method num_threads_opt : Rdf_term.literal option
    method num_views : Rdf_term.literal list
    method num_views_opt : Rdf_term.literal option
    method owner_of : Iri.t list
    method owner_of_opt : Iri.t option
    method parent_of : Iri.t list
    method parent_of_opt : Iri.t option
    method part_of : Iri.t list
    method part_of_opt : Iri.t option
    method previous_by_date : Iri.t list
    method previous_by_date_opt : Iri.t option
    method previous_version : Iri.t list
    method previous_version_opt : Iri.t option
    method reference : Iri.t list
    method reference_opt : Iri.t option
    method related_to : Iri.t list
    method related_to_opt : Iri.t option
    method reply_of : Iri.t list
    method reply_of_opt : Iri.t option
    method scope_of : Iri.t list
    method scope_of_opt : Iri.t option
    method space_of : Iri.t list
    method space_of_opt : Iri.t option
    method subject : Rdf_term.literal list
    method subject_opt : Rdf_term.literal option
    method subscriber_of : Iri.t list
    method subscriber_of_opt : Iri.t option
    method title : Rdf_term.literal list
    method title_opt : Rdf_term.literal option
    method topic : Iri.t list
    method topic_opt : Iri.t option
    method usergroup_of : Iri.t list
    method usergroup_of_opt : Iri.t option
  end

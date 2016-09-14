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

(** The content of the Item in plain text format. *)
val content : Iri.t

(** The encoded content of the Post, contained in CDATA areas. *)
val content_encoded : Iri.t

(** When this was created, in ISO 8601 format. *)
val created_at : Iri.t

(** The content of the Post. *)
val description : Iri.t

(** An electronic mail address of the UserAccount, encoded using SHA1. *)
val email_sha1 : Iri.t

(** First (real) name of this User. Synonyms include given name or christian name. *)
val first_name : Iri.t

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

(** When this was modified, in ISO 8601 format. *)
val modified_at : Iri.t

(** The name of a SIOC concept instance, e.g. a username for a UserAccount, group name for a Usergroup, etc. *)
val name : Iri.t

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

(** Keyword(s) describing subject of the Post. *)
val subject : Iri.t

(** This is the title (subject line) of the Post. Note that for a Post within a threaded discussion that has no parents, it would detail the topic thread. *)
val title : Iri.t


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

  (** The content of the Item in plain text format. *)
  val sioc_content : Iri.t

  (** The encoded content of the Post, contained in CDATA areas. *)
  val sioc_content_encoded : Iri.t

  (** When this was created, in ISO 8601 format. *)
  val sioc_created_at : Iri.t

  (** The content of the Post. *)
  val sioc_description : Iri.t

  (** An electronic mail address of the UserAccount, encoded using SHA1. *)
  val sioc_email_sha1 : Iri.t

  (** First (real) name of this User. Synonyms include given name or christian name. *)
  val sioc_first_name : Iri.t

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

  (** When this was modified, in ISO 8601 format. *)
  val sioc_modified_at : Iri.t

  (** The name of a SIOC concept instance, e.g. a username for a UserAccount, group name for a Usergroup, etc. *)
  val sioc_name : Iri.t

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

  (** Keyword(s) describing subject of the Post. *)
  val sioc_subject : Iri.t

  (** This is the title (subject line) of the Post. Note that for a Post within a threaded discussion that has no parents, it would detail the topic thread. *)
  val sioc_title : Iri.t

end

class from : ?sub: Iri.t -> Rdf_graph.graph ->
  object
    method content : Rdf_term.literal list
    method content_encoded : Rdf_term.literal list
    method created_at : Rdf_term.literal list
    method description : Rdf_term.literal list
    method email_sha1 : Rdf_term.literal list
    method first_name : Rdf_term.literal list
    method id : Rdf_term.literal list
    method ip_address : Rdf_term.literal list
    method last_activity_date : Rdf_term.literal list
    method last_item_date : Rdf_term.literal list
    method last_name : Rdf_term.literal list
    method last_reply_date : Rdf_term.literal list
    method modified_at : Rdf_term.literal list
    method name : Rdf_term.literal list
    method note : Rdf_term.literal list
    method num_authors : Iri.t list
    method num_items : Iri.t list
    method num_replies : Iri.t list
    method num_threads : Iri.t list
    method num_views : Iri.t list
    method subject : Rdf_term.literal list
    method title : Rdf_term.literal list
  end

(** Elements of [http://rdfs.org/sioc/ns#] *)

(** [http://rdfs.org/sioc/ns#] *)
val sioc : Iri.t
val sioc_ : string -> Iri.t

(** Community is a high-level concept that defines an online community and what it consists of. *)
val community : Iri.t

(** An area in which content Items are contained. *)
val container : Iri.t

(** A discussion area on which Posts or entries are made. *)
val forum : Iri.t

(** An Item is something which can be in a Container. *)
val item : Iri.t

(** An article or message that can be posted to a Forum. *)
val post : Iri.t

(** A Role is a function of a UserAccount within a scope of a particular Forum, Site, etc. *)
val role : Iri.t

(** A Site can be the location of an online community or set of communities, with UserAccounts and Usergroups creating Items in a set of Containers. It can be thought of as a web-accessible data Space. *)
val site : Iri.t

(** A Space is a place where data resides, e.g. on a website, desktop, fileshare, etc. *)
val space : Iri.t

(** A container for a series of threaded discussion Posts or Items. *)
val thread : Iri.t

(** UserAccount is now preferred. This is a deprecated class for a User in an online community site. *)
val c_User : Iri.t

(** A user account in an online community site. *)
val userAccount : Iri.t

(** A set of UserAccounts whose owners have a common purpose or interest. Can be used for access control purposes. *)
val usergroup : Iri.t


module Open : sig
  (** Community is a high-level concept that defines an online community and what it consists of. *)
  val sioc_community : Iri.t

  (** An area in which content Items are contained. *)
  val sioc_container : Iri.t

  (** A discussion area on which Posts or entries are made. *)
  val sioc_forum : Iri.t

  (** An Item is something which can be in a Container. *)
  val sioc_item : Iri.t

  (** An article or message that can be posted to a Forum. *)
  val sioc_post : Iri.t

  (** A Role is a function of a UserAccount within a scope of a particular Forum, Site, etc. *)
  val sioc_role : Iri.t

  (** A Site can be the location of an online community or set of communities, with UserAccounts and Usergroups creating Items in a set of Containers. It can be thought of as a web-accessible data Space. *)
  val sioc_site : Iri.t

  (** A Space is a place where data resides, e.g. on a website, desktop, fileshare, etc. *)
  val sioc_space : Iri.t

  (** A container for a series of threaded discussion Posts or Items. *)
  val sioc_thread : Iri.t

  (** UserAccount is now preferred. This is a deprecated class for a User in an online community site. *)
  val sioc_c_User : Iri.t

  (** A user account in an online community site. *)
  val sioc_userAccount : Iri.t

  (** A set of UserAccounts whose owners have a common purpose or interest. Can be used for access control purposes. *)
  val sioc_usergroup : Iri.t

end

class from : ?sub: Iri.t -> Rdf_graph.graph ->
  object
    method community : Iri.t list
    method container : Iri.t list
    method forum : Iri.t list
    method item : Iri.t list
    method post : Iri.t list
    method role : Iri.t list
    method site : Iri.t list
    method space : Iri.t list
    method thread : Iri.t list
    method userAccount : Iri.t list
    method usergroup : Iri.t list
  end

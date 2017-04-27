(** Elements of [http://www.w3.org/ns/auth/acl#] *)

(** [http://www.w3.org/ns/auth/acl#] *)
val acl : Iri.t
val acl_ : string -> Iri.t

(** Any kind of access to a resource. Don't use this, use R W and RW *)
val c_Access : Iri.t

(** Append accesses are specific write access which only add information, and do not remove information. For text files, for example, append access allows bytes to be added onto the end of the file. For RDF graphs, Append access allows adds triples to the graph but does not remove any. Append access is useful for dropbox functionality. Dropbox can be used for link notification, which the information added is a notification that a some link has been made elsewhere relevant to the given resource. *)
val c_Append : Iri.t

(** An element of access control, allowing agent to agents access of some kind to resources or classes of resources *)
val c_Authorization : Iri.t

(** Allows read/write access to the ACL for the resource(s) *)
val c_Control : Iri.t

(** The class of read operations *)
val c_Read : Iri.t

val c_Write : Iri.t

(** The Access Control file for this information resource. This may of course be a virtual resorce implemented by the access control system. Note also HTTP's header Link: foo.meta ;rel=meta can be used for this. *)
val accessControl : Iri.t

(** The information resource to which access is being granted. *)
val accessTo : Iri.t

(** A class of information resources to which access is being granted. *)
val accessToClass : Iri.t

(** A person or social entity to being given the right *)
val agent : Iri.t

(** A class of persons or social entities to being given the right *)
val agentClass : Iri.t

(** A group of persons or social entities to being given the right *)
val agentGroup : Iri.t

(** A directory for which this authorization is used for new files in the directory. *)
val defaultForNew : Iri.t

(** Delegates a person or another agent to act on behalf of the agent. For example, Alice delegates Bob to act on behalf of Alice for ACL purposes. *)
val delegates : Iri.t

(** A mode of access such as read or write. *)
val mode : Iri.t

(** The person or other agent which owns this. For example, the owner of a file in a filesystem. There is a sense of right to control. Typically defaults to the agent who craeted something but can be changed. *)
val owner : Iri.t


module Open : sig
  (** Any kind of access to a resource. Don't use this, use R W and RW *)
  val acl_c_Access : Iri.t

  (** Append accesses are specific write access which only add information, and do not remove information. For text files, for example, append access allows bytes to be added onto the end of the file. For RDF graphs, Append access allows adds triples to the graph but does not remove any. Append access is useful for dropbox functionality. Dropbox can be used for link notification, which the information added is a notification that a some link has been made elsewhere relevant to the given resource. *)
  val acl_c_Append : Iri.t

  (** An element of access control, allowing agent to agents access of some kind to resources or classes of resources *)
  val acl_c_Authorization : Iri.t

  (** Allows read/write access to the ACL for the resource(s) *)
  val acl_c_Control : Iri.t

  (** The class of read operations *)
  val acl_c_Read : Iri.t

  val acl_c_Write : Iri.t

  (** The Access Control file for this information resource. This may of course be a virtual resorce implemented by the access control system. Note also HTTP's header Link: foo.meta ;rel=meta can be used for this. *)
  val acl_accessControl : Iri.t

  (** The information resource to which access is being granted. *)
  val acl_accessTo : Iri.t

  (** A class of information resources to which access is being granted. *)
  val acl_accessToClass : Iri.t

  (** A person or social entity to being given the right *)
  val acl_agent : Iri.t

  (** A class of persons or social entities to being given the right *)
  val acl_agentClass : Iri.t

  (** A group of persons or social entities to being given the right *)
  val acl_agentGroup : Iri.t

  (** A directory for which this authorization is used for new files in the directory. *)
  val acl_defaultForNew : Iri.t

  (** Delegates a person or another agent to act on behalf of the agent. For example, Alice delegates Bob to act on behalf of Alice for ACL purposes. *)
  val acl_delegates : Iri.t

  (** A mode of access such as read or write. *)
  val acl_mode : Iri.t

  (** The person or other agent which owns this. For example, the owner of a file in a filesystem. There is a sense of right to control. Typically defaults to the agent who craeted something but can be changed. *)
  val acl_owner : Iri.t

end

class from : ?sub: Rdf_term.term -> Rdf_graph.graph ->
  object
    method accessControl : Rdf_term.term list
    method accessControl_opt : Rdf_term.term option
    method accessControl_iris : Iri.t list
    method accessControl_opt_iri : Iri.t option
    method accessTo : Rdf_term.term list
    method accessTo_opt : Rdf_term.term option
    method accessTo_iris : Iri.t list
    method accessTo_opt_iri : Iri.t option
    method accessToClass : Rdf_term.term list
    method accessToClass_opt : Rdf_term.term option
    method accessToClass_iris : Iri.t list
    method accessToClass_opt_iri : Iri.t option
    method agent : Rdf_term.term list
    method agent_opt : Rdf_term.term option
    method agent_iris : Iri.t list
    method agent_opt_iri : Iri.t option
    method agentClass : Rdf_term.term list
    method agentClass_opt : Rdf_term.term option
    method agentClass_iris : Iri.t list
    method agentClass_opt_iri : Iri.t option
    method agentGroup : Rdf_term.term list
    method agentGroup_opt : Rdf_term.term option
    method agentGroup_iris : Iri.t list
    method agentGroup_opt_iri : Iri.t option
    method defaultForNew : Rdf_term.term list
    method defaultForNew_opt : Rdf_term.term option
    method defaultForNew_iris : Iri.t list
    method defaultForNew_opt_iri : Iri.t option
    method delegates : Rdf_term.term list
    method delegates_opt : Rdf_term.term option
    method delegates_iris : Iri.t list
    method delegates_opt_iri : Iri.t option
    method mode : Rdf_term.term list
    method mode_opt : Rdf_term.term option
    method mode_iris : Iri.t list
    method mode_opt_iri : Iri.t option
    method owner : Rdf_term.term list
    method owner_opt : Rdf_term.term option
    method owner_iris : Iri.t list
    method owner_opt_iri : Iri.t option
  end

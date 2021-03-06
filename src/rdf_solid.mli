(** Elements of [http://www.w3.org/ns/solid/terms#] *)

(** [http://www.w3.org/ns/solid/terms#] *)
val solid : Iri.t
val solid_ : string -> Iri.t

(** A Solid account. *)
val c_Account : Iri.t

(** A resource containing notifications. *)
val c_Inbox : Iri.t

(** A notification resource. *)
val c_Notification : Iri.t

(** A resource containing time ordered items and sub-containers.  Sub-containers may be desirable in file based systems to split the timeline into logical components e.g. /yyyy-mm-dd/ as used in ISO 8061. *)
val c_Timeline : Iri.t

(** A index of type registries for resources. Applications can register the RDF type they use and list them in the index resource. *)
val c_TypeIndex : Iri.t

(** A solid account belonging to an Agent. *)
val account : Iri.t

(** Inbox resource for notifications. *)
val inbox : Iri.t

(** Notification resource for an inbox. *)
val notification : Iri.t

(** Indicates if a message has been read or not. This property should have a boolean datatype. *)
val read : Iri.t

(** Timeline for a given resource. *)
val timeline : Iri.t

(** Points to a TypeIndex resource. *)
val typeIndex : Iri.t


module Open : sig
  (** A Solid account. *)
  val solid_c_Account : Iri.t

  (** A resource containing notifications. *)
  val solid_c_Inbox : Iri.t

  (** A notification resource. *)
  val solid_c_Notification : Iri.t

  (** A resource containing time ordered items and sub-containers.  Sub-containers may be desirable in file based systems to split the timeline into logical components e.g. /yyyy-mm-dd/ as used in ISO 8061. *)
  val solid_c_Timeline : Iri.t

  (** A index of type registries for resources. Applications can register the RDF type they use and list them in the index resource. *)
  val solid_c_TypeIndex : Iri.t

  (** A solid account belonging to an Agent. *)
  val solid_account : Iri.t

  (** Inbox resource for notifications. *)
  val solid_inbox : Iri.t

  (** Notification resource for an inbox. *)
  val solid_notification : Iri.t

  (** Indicates if a message has been read or not. This property should have a boolean datatype. *)
  val solid_read : Iri.t

  (** Timeline for a given resource. *)
  val solid_timeline : Iri.t

  (** Points to a TypeIndex resource. *)
  val solid_typeIndex : Iri.t

end

class from : ?sub: Rdf_term.term -> Rdf_graph.graph ->
  object
    method account : Rdf_term.term list
    method account_opt : Rdf_term.term option
    method account_iris : Iri.t list
    method account_opt_iri : Iri.t option
    method inbox : Rdf_term.term list
    method inbox_opt : Rdf_term.term option
    method inbox_iris : Iri.t list
    method inbox_opt_iri : Iri.t option
    method notification : Rdf_term.term list
    method notification_opt : Rdf_term.term option
    method notification_iris : Iri.t list
    method notification_opt_iri : Iri.t option
    method read : Rdf_term.term list
    method read_opt : Rdf_term.term option
    method read_iris : Iri.t list
    method read_opt_iri : Iri.t option
    method timeline : Rdf_term.term list
    method timeline_opt : Rdf_term.term option
    method timeline_iris : Iri.t list
    method timeline_opt_iri : Iri.t option
    method typeIndex : Rdf_term.term list
    method typeIndex_opt : Rdf_term.term option
    method typeIndex_iris : Iri.t list
    method typeIndex_opt_iri : Iri.t option
  end

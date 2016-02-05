(** Elements of [http://www.w3.org/ns/pim/space#] *)

(** [http://www.w3.org/ns/pim/space#] *)
val pim : Iri.t
val pim_ : string -> Iri.t

(** A  storage is a space of URIs in which you can individually control for each resource
    who has access to it.
 *)
val controlledStorage : Iri.t

(** A personal storage is a space of URIs in which you and only you have access to data,
    you cannot give access to anyone else.
 *)
val personalStorage : Iri.t

(** A public storage is a space of URIs in which you have access to data,
    and all data is accessible to anyone without control.
 *)
val publicStorage : Iri.t

(** A storage is a space of URIs in which you have access to data.
 *)
val storage : Iri.t

(** Workspaces are place where data is stored, and associated polices of privacy.
A given application typically stores information in several different
workspaces, some being user private, some shared, and some public.
 *)
val workspace : Iri.t


module Open : sig
  (** A  storage is a space of URIs in which you can individually control for each resource
    who has access to it.
 *)
  val pim_controlledStorage : Iri.t

  (** A personal storage is a space of URIs in which you and only you have access to data,
    you cannot give access to anyone else.
 *)
  val pim_personalStorage : Iri.t

  (** A public storage is a space of URIs in which you have access to data,
    and all data is accessible to anyone without control.
 *)
  val pim_publicStorage : Iri.t

  (** A storage is a space of URIs in which you have access to data.
 *)
  val pim_storage : Iri.t

  (** Workspaces are place where data is stored, and associated polices of privacy.
A given application typically stores information in several different
workspaces, some being user private, some shared, and some public.
 *)
  val pim_workspace : Iri.t

end

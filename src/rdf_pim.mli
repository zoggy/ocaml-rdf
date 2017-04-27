(** Elements of [http://www.w3.org/ns/pim/space#] *)

(** [http://www.w3.org/ns/pim/space#] *)
val pim : Iri.t
val pim_ : string -> Iri.t

val c_ConfigurationFile : Iri.t

(** A  storage is a space of URIs in which you can individually control for each resource
    who has access to it.
 *)
val c_ControlledStorage : Iri.t

(** This is a workspace for storing the 
    information about the other workspaces.
    As a user, you normally don't have to worry about it. *)
val c_MasterWorkspace : Iri.t

(** A personal storage is a space of URIs in which you and only you have access to data,
    you cannot give access to anyone else.
 *)
val c_PersonalStorage : Iri.t

(** AAceess may not be open to the public. 
    Contains preferences resources *)
val c_PreferencesWorkspace : Iri.t

(** Access only by the you, the user. *)
val c_PrivateWorkspace : Iri.t

(** A public storage is a space of URIs in which you have access to data,
    and all data is accessible to anyone without control.
 *)
val c_PublicStorage : Iri.t

(** Aceess is open to the public. Anything in a public workspace
    can be accesed by anyone. *)
val c_PublicWorkspace : Iri.t

(** Access is to some but not all people. *)
val c_SharedWorkspace : Iri.t

(** A storage is a space of URIs in which you have access to data.
 *)
val c_Storage : Iri.t

(** Workspaces are place where data is stored, and associated polices of privacy.
A given application typically stores information in several different
workspaces, some being user private, some shared, and some public.
 *)
val c_Workspace : Iri.t

val masterWorkspace : Iri.t

val preferencesFile : Iri.t

(** The storage in which this workspace is *)
val storage : Iri.t

(** URIs which start with this string are in this workspace or storage.
This may be used for constructing URIs for new storage resources.
 *)
val uriPrefix : Iri.t

val workspace : Iri.t


module Open : sig
  val pim_c_ConfigurationFile : Iri.t

  (** A  storage is a space of URIs in which you can individually control for each resource
    who has access to it.
 *)
  val pim_c_ControlledStorage : Iri.t

  (** This is a workspace for storing the 
    information about the other workspaces.
    As a user, you normally don't have to worry about it. *)
  val pim_c_MasterWorkspace : Iri.t

  (** A personal storage is a space of URIs in which you and only you have access to data,
    you cannot give access to anyone else.
 *)
  val pim_c_PersonalStorage : Iri.t

  (** AAceess may not be open to the public. 
    Contains preferences resources *)
  val pim_c_PreferencesWorkspace : Iri.t

  (** Access only by the you, the user. *)
  val pim_c_PrivateWorkspace : Iri.t

  (** A public storage is a space of URIs in which you have access to data,
    and all data is accessible to anyone without control.
 *)
  val pim_c_PublicStorage : Iri.t

  (** Aceess is open to the public. Anything in a public workspace
    can be accesed by anyone. *)
  val pim_c_PublicWorkspace : Iri.t

  (** Access is to some but not all people. *)
  val pim_c_SharedWorkspace : Iri.t

  (** A storage is a space of URIs in which you have access to data.
 *)
  val pim_c_Storage : Iri.t

  (** Workspaces are place where data is stored, and associated polices of privacy.
A given application typically stores information in several different
workspaces, some being user private, some shared, and some public.
 *)
  val pim_c_Workspace : Iri.t

  val pim_masterWorkspace : Iri.t

  val pim_preferencesFile : Iri.t

  (** The storage in which this workspace is *)
  val pim_storage : Iri.t

  (** URIs which start with this string are in this workspace or storage.
This may be used for constructing URIs for new storage resources.
 *)
  val pim_uriPrefix : Iri.t

  val pim_workspace : Iri.t

end

class from : ?sub: Rdf_term.term -> Rdf_graph.graph ->
  object
    method masterWorkspace : Rdf_term.term list
    method masterWorkspace_opt : Rdf_term.term option
    method masterWorkspace_iris : Iri.t list
    method masterWorkspace_opt_iri : Iri.t option
    method preferencesFile : Rdf_term.term list
    method preferencesFile_opt : Rdf_term.term option
    method preferencesFile_iris : Iri.t list
    method preferencesFile_opt_iri : Iri.t option
    method storage : Rdf_term.term list
    method storage_opt : Rdf_term.term option
    method storage_iris : Iri.t list
    method storage_opt_iri : Iri.t option
    method uriPrefix : Rdf_term.term list
    method uriPrefix_opt : Rdf_term.term option
    method uriPrefix_iris : Iri.t list
    method uriPrefix_opt_iri : Iri.t option
    method workspace : Rdf_term.term list
    method workspace_opt : Rdf_term.term option
    method workspace_iris : Iri.t list
    method workspace_opt_iri : Iri.t option
  end

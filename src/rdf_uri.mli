(** URIs. *)

(** URIs are abstract. {b Do not compare with generic comparison
  functions} ([Pervasives.compare], (=), ...) as it contains
  functional values. Use {!equal} or {!compare}. *)
type uri

(** Create a string from a URI. *)
val string : uri -> string

(** Create a URI from a string. *)
val uri : string -> uri

(** Add the given string to the path of the given URI, using '/' as separator. *)
val concat : uri -> string -> uri

(** Return a new URI with the path modified to parent path of the original URI. *)
val parent : uri -> uri

(** Modify the fragment part of the URI. *)
val set_fragment : uri -> string -> uri

(** Get the path part of the URI. *)
val path : uri -> string list

(** Comparison of two URIs, as usual. *)
val compare : uri -> uri -> int

(** Equality over URIs. *)
val equal : uri -> uri -> bool

(** Get a {!Neturl.url} from the given URI. ([Neturl.url]
  is the underlying represention of URIs). *)
val neturl : uri -> Neturl.url

(** Get a {!uri} from the given {!Neturl.url}. *)
val of_neturl : Neturl.url -> uri
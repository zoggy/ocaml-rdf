(** URIs. *)

type uri

val string : uri -> string
val uri : string -> uri

(* add the given string to the path of the given uri, using '/' as separator. *)
val concat : uri -> string -> uri

(* return a new uri with the path modified to parent path of the original uri. *)
val parent : uri -> uri

(* modify the fragment part of the uri. *)
val set_fragment : uri -> string -> uri

(* get the path part of the uri *)
val path : uri -> string list

val compare : uri -> uri -> int
val equal : uri -> uri -> bool

val neturl : uri -> Neturl.url
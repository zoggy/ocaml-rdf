(** Misc functions. *)

val string_of_opt : string option -> string
val opt_of_string : string -> string option
val map_opt : ('a -> 'b) -> 'a option -> 'b option
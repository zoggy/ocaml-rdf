(** Misc functions. *)

val string_of_opt : string option -> string
val opt_of_string : string -> string option
val map_opt : ('a -> 'b) -> 'a option -> 'b option

(** Creating a log function.
  [create_log_fun env_var] get the log level (an integer) from the given
  environment variable, and returns a function to print messages.
  This function takes a level (default is 1) and a function returning
  the message do print. The function is called only if the log level is
  higher than or equal to the given level.
  The [loc] parameter of the returned function can be used to indicate
  an additional string to print before the log message.
  If the environment variable is empty or does not contain an integer,
  then the log level is set to 0.
  @param prefix can be used to indicate a string prefixing every message
  @param print can be given to the function build the log function, to
  indicate an alternative way to display the message; default is to call
  [prerr_endline].
  *)
val create_log_fun :
  ?prefix: string ->
  ?print:(string -> unit) -> string ->
  (?loc: string -> ?level:int -> (unit -> string) -> unit)

(** Same as [create_log_fun] but also return a function to change
       the log level.*)
val create_log_fun_with_set :
  ?prefix: string ->
  ?print:(string -> unit) -> string ->
  (?loc: string -> ?level:int -> (unit -> string) -> unit) *
  (int -> unit)


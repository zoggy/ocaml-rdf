exception Not_implemented of string
val not_impl : string -> 'a

val sha1 : (string -> string) ref
val sha256 : (string -> string) ref

val pcre_replace :
  (flags:Pcre.cflag list -> pat:string -> templ:string -> string -> string)
  ref
val pcre_pmatch : (flags:Pcre.cflag list -> pat:string -> string -> bool) ref

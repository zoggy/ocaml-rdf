
(** *)

exception Not_implemented of string
let not_impl str = raise (Not_implemented str)

let sha1 : (string -> string) ref = ref (fun _ -> not_impl "sha1")
let sha256 : (string -> string) ref = ref (fun _ -> not_impl "sha256")

let pcre_replace :
  (flags: Pcre.cflag list -> pat: string -> templ:string -> string -> string) ref =
  ref (fun ~flags ~pat ~templ str -> not_impl "pcre_replace")

let pcre_pmatch :
  (flags: Pcre.cflag list -> pat: string -> string -> bool) ref =
  ref (fun ~flags ~pat str -> not_impl "pcre_pmath")
  
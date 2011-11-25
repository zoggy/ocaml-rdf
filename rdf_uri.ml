(** *)

open Rdf_types;;

external new_uri : world -> string -> uri option = "ml_librdf_new_uri"
external new_uri2 : world -> string -> int -> uri option = "ml_librdf_new_uri2"

external new_uri_from_uri : uri -> uri option = "ml_librdf_new_uri_from_uri"
external new_uri_from_uri_local_name : uri -> string -> uri option = "ml_librdf_new_uri_from_uri_local_name"

external free_uri : uri -> unit = "ml_librdf_free_uri"

external uri_as_string : uri -> string = "ml_librdf_uri_as_string"
external uri_equals : uri -> uri -> bool = "ml_uri_equals"

external uri_is_file : uri -> bool = "ml_uri_is_file_uri"
external uri_to_filename : uri -> string option = "ml_uri_to_filename"

external new_uri_normalised_to_base : string -> uri -> uri -> uri option = "ml_new_uri_normalised_to_base"
external new_uri_relative_to_base : uri -> string -> uri option = "ml_new_uri_relative_to_base"
external new_uri_from_filename : world -> string -> uri option = "ml_new_uri_from_filename"

external uri_compare : uri -> uri -> int = "ml_uri_compare"

external pointer_of_uri : uri -> Nativeint.t = "ml_pointer_of_custom"

let (add_uri, incr_uri, decr_uri) =
  Rdf_misc.create_pointer_counter "uri"
  pointer_of_uri free_uri;;

let on_new_uri u = Rdf_misc.do_opt add_uri u; u;;

let new_uri world string = on_new_uri (new_uri world string);;
let new_uri2 world string n = on_new_uri (new_uri2 world string n);;

let new_uri_from_uri uri = on_new_uri (new_uri_from_uri uri);;
let new_uri_from_uri_local_name uri name =
  on_new_uri (new_uri_from_uri_local_name uri name);;

let new_uri_normalised_to_base string ~source ~base =
  on_new_uri (new_uri_normalised_to_base string source base)
;;
let new_uri_relative_to_base base string =
  on_new_uri (new_uri_relative_to_base base string)
;;
let new_uri_from_filename world string =
  on_new_uri (new_uri_from_filename world string)
;;

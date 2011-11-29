(** *)

open Rdf_types;;

(**/**)
module Raw =
  struct
(*
    external new_hash : world -> string -> hash option = "ml_librdf_new_hash"
*)
    external new_hash_from_hash :
      hash -> hash option = "ml_librdf_new_hash_from_hash"
    external new_hash_from_string : world ->
      string -> string -> hash option = "ml_librdf_new_hash_from_string"
    external free_hash : hash -> unit = "ml_librdf_free_hash"

    external hash_get : hash -> string -> string option = "ml_librdf_hash_get"
    external hash_get_as_boolean : hash -> string -> int = "ml_librdf_hash_get_as_boolean"
    external hash_get_as_long : hash -> string -> int = "ml_librdf_hash_get_as_long"
    external hash_get_del : hash -> string -> string option = "ml_librdf_hash_get_del"

    external hash_put_string : hash -> string -> string -> bool =
      "ml_librdf_hash_put_strings"

    external hash_interpret_template : string -> hash -> string -> string -> string =
      "ml_librdf_hash_interpret_template"

    external hash_from_string : hash -> string -> bool =
      "ml_librdf_hash_from_string"
    external hash_to_string : hash -> int -> string =
      "ml_librdf_hash_to_string"

    external pointer_of_hash : hash -> Nativeint.t = "ml_pointer_of_custom"
   end

let hash_to_finalise v = Gc.finalise Raw.free_hash v;;
(**/**)

exception Hash_creation_failed of string;;

let on_new_hash fun_name = function
  None -> raise (Hash_creation_failed fun_name)
| Some n -> hash_to_finalise n; n
;;

(*
let new_hash world name =
  on_new_hash "" (Raw.new_hash world name)
;;
*)
let copy_hash hash =
  on_new_hash "from_hash" (Raw.new_hash_from_hash hash)
;;

let new_hash_from_string world ~name ~string =
  on_new_hash "from_string" (Raw.new_hash_from_string world name string)
;;

let has_get_as_boolean hash string =
  let n = Raw.hash_get_as_boolean hash string in
  if n < 0 then raise Not_found;
  n > 0
;;

let has_get_as_long hash string =
  let n = Raw.hash_get_as_long hash string in
  if n < 0 then raise Not_found;
  n
;;

let hash_put_string hash ~key ~value =
  match Raw.hash_put_string hash key value with
    false -> failwith "hash_put_strings"
  | true -> ()
;;

let hash_interpret_template hash ~template ~prefix ~suffix =
  Raw.hash_interpret_template template hash prefix suffix
;;

let hash_from_string hash s =
  if not (Raw.  hash_from_string hash s) then
    failwith "hash_from_string"
;;

let hash_to_string hash = Raw.hash_to_string hash 0
;;

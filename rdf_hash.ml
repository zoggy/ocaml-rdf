(** *)

open Rdf_types;;

let dbg = Rdf_misc.create_log_fun ~prefix: "Rdf_hash" "ORDF_HASH";;

(**/**)
module Raw =
  struct
(*
    external new_hash : world -> string -> hash option = "ml_librdf_new_hash"
*)
    external new_from_hash :
      hash -> hash option = "ml_librdf_new_hash_from_hash"
    external new_from_string : world ->
      string -> string -> hash option = "ml_librdf_new_hash_from_string"
    external free : hash -> unit = "ml_librdf_free_hash"

    external get : hash -> string -> string option = "ml_librdf_hash_get"
    external get_as_boolean : hash -> string -> int = "ml_librdf_hash_get_as_boolean"
    external get_as_long : hash -> string -> int = "ml_librdf_hash_get_as_long"
    external get_del : hash -> string -> string option = "ml_librdf_hash_get_del"

    external put_strings : hash -> string -> string -> int =
      "ml_librdf_hash_put_strings"

    external interpret_template : string -> hash -> string -> string -> string =
      "ml_librdf_hash_interpret_template"

    external from_string : hash -> string -> int =
      "ml_librdf_hash_from_string"
(*
    external to_string : hash -> int -> string =
      "ml_librdf_hash_to_string"
*)
    external pointer_of_hash : hash -> Nativeint.t = "ml_pointer_of_custom"
   end

let free v =
  dbg (fun () -> Printf.sprintf "Freeing hash %s"
   (Nativeint.to_string (Raw.pointer_of_hash v)));
  Raw.free v
;;
let to_finalise v = Gc.finalise free v;;
(**/**)

exception Hash_creation_failed of string;;

let on_new_hash fun_name = function
  None -> raise (Hash_creation_failed fun_name)
| Some n -> to_finalise n; n
;;

(*
let new_hash world name =
  on_new_hash "" (Raw.new_hash world name)
;;
*)
let copy_hash hash =
  on_new_hash "from_hash" (Raw.new_from_hash hash)
;;

let new_from_string world ~name ~string =
  on_new_hash "from_string" (Raw.new_from_string world name string)
;;

let get = Raw.get;;

let has_get_as_boolean hash string =
  let n = Raw.get_as_boolean hash string in
  if n < 0 then raise Not_found;
  n > 0
;;

let has_get_as_long hash string =
  let n = Raw.get_as_long hash string in
  if n < 0 then raise Not_found;
  n
;;

let put_strings hash ~key ~value =
  match Raw.put_strings hash key value with
  | 0 -> ()
  | _ -> failwith "hash_put_strings"
;;

let interpret_template hash ~template ~prefix ~suffix =
  Raw.interpret_template template hash prefix suffix
;;

let from_string hash s =
  match Raw.from_string hash s with
    0 -> ()
  | _ -> failwith "hash_from_string"
;;
(*
let to_string hash = Raw.to_string hash 0
;;
*)
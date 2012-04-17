(** *)

type uri = string
type literal = string * uri option
type blank_id = string

type node =
  | Uri of uri
  | Literal of literal
  | Blank
  | Blank_ of blank_id

type triple = node * node * node


let node_of_uri_string s = Uri s;;
let node_of_literal_string ?typ s = Literal (s, typ)

type options = (string * string) list
let get_option ?def name l =
  try List.assoc name l
  with Not_found ->
    match def with
        None -> failwith (Printf.sprintf "Missing option %S" name)
      | Some v -> v
;;


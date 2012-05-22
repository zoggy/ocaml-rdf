(** *)


type literal = {
    lit_value : string ;
    lit_language : string option ;
    lit_type : Rdf_uri.uri option ;
  }
type blank_id = string

type node =
  | Uri of Rdf_uri.uri
  | Literal of literal
  | Blank
  | Blank_ of blank_id

type triple = node * node * node

let string_of_blank_id id = id;;
let blank_id_of_string str = str;;

let node_of_uri_string s = Uri (Rdf_uri.uri s);;
let mk_literal ?typ ?lang v =
  { lit_value = v ; lit_language = lang ; lit_type = typ ; }
;;
let node_of_literal_string ?typ ?lang v =
  Literal (mk_literal ?typ ?lang v)
;;

let mk_literal_datetime ?(d=Unix.time()) () =
  let v = Netdate.mk_internet_date d in
  mk_literal ~typ: (Rdf_uri.uri "http://www.w3.org/2001/XMLSchema#dateTime") v
;;

let node_of_datetime ?d () =
  Literal (mk_literal_datetime ?d ())
;;

let datetime_of_literal lit = Netdate.parse lit.lit_value;;

let mk_literal_bool b =
  let v = if b then "1" else "0" in
  mk_literal ~typ: (Rdf_uri.uri "http://www.w3.org/2001/XMLSchema#boolean") v
;;

let node_of_bool b = Literal (mk_literal_bool b);;

let bool_of_literal lit =
  match lit.lit_value with
    "1" | "true" -> true
  | _ -> false
;;


let string_of_node = function
| Uri uri -> Printf.sprintf "<%s>" (Rdf_uri.string uri)
| Literal lit ->
    Printf.sprintf "%S%s%s"
    lit.lit_value
    (match lit.lit_language with
       None -> ""
     | Some l -> Printf.sprintf "@%s" l)
    (match lit.lit_type with
       None -> ""
     | Some t -> Printf.sprintf "^^%s" (Rdf_uri.string t))
| Blank -> "_"
| Blank_ id -> Printf.sprintf "_:%s" (string_of_blank_id id)
;;





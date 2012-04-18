(** *)

let syntax =
  let stx = Hashtbl.find Neturl.common_url_syntax "http" in
  let stx = Neturl.partial_url_syntax stx in
  { stx with Neturl.url_enable_fragment = Neturl.Url_part_allowed }
;;

type uri = Neturl.url;;

let string uri = Neturl.string_of_url uri;;
let uri = Neturl.url_of_string syntax;;

let concat uri s =
  let path = (Neturl.url_path uri)@[s] in
  Neturl.modify_url ~path uri
;;

let parent uri =
  let path = Neturl.url_path uri in
  let path =
    match List.rev path with
      [] -> []
    | _ :: q -> List.rev q
  in
  Neturl.modify_url ~path uri
;;

let set_fragment uri fragment =
  Neturl.modify_url ~fragment uri
;;

let path uri = Neturl.url_path uri;;

let compare uri1 uri2 = Pervasives.compare (string uri1) (string uri2);;
let equal u1 u2 = compare u1 u2 = 0;;

let neturl uri = uri;;

  
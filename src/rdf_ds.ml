(** Datasets. *)

module Irimap = Map.Make
  (struct type t = Rdf_uri.uri let compare = Rdf_uri.compare end)
module Iriset = Set.Make
  (struct type t = Rdf_uri.uri let compare = Rdf_uri.compare end)

exception Could_not_retrieve_graph of Rdf_uri.uri * string
let could_not_retrieve_graph uri msg =
  raise (Could_not_retrieve_graph (uri, msg))
;;

type dataset =
  { default : Rdf_graph.graph ;
    get_named : Rdf_uri.uri -> Rdf_graph.graph ;
  }

let base_dataset ?(named=[]) default =
  let named = List.fold_left (fun map (uri,g) -> Irimap.add uri g map) Irimap.empty named in
  let get_named uri =
    try Irimap.find uri named
    with Not_found ->
        could_not_retrieve_graph uri
          ("Unknown graph "^(Rdf_uri.string uri))
  in
  { default ; get_named }
;;

let dataset ?get_named default =
  match get_named with
    None -> base_dataset default
  | Some get_named -> { default ; get_named }
;;

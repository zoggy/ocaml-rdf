(** Datasets. *)

module Irimap = Rdf_uri.Urimap
module Iriset = Rdf_uri.Uriset

exception Could_not_retrieve_graph of Rdf_uri.uri * string
let could_not_retrieve_graph uri msg =
  raise (Could_not_retrieve_graph (uri, msg))
;;

type dataset =
  { default : Rdf_graph.graph ;
    named : Iriset.t ;
    get_named : Rdf_uri.uri -> Rdf_graph.graph ;
  }

let simple_dataset ?(named=[]) default =
  let named_set = List.fold_left (fun set (uri,_) -> Iriset.add uri set) Iriset.empty named in
  let named = List.fold_left (fun map (uri,g) -> Irimap.add uri g map) Irimap.empty named in
  let get_named uri =
    try Irimap.find uri named
    with Not_found ->
        could_not_retrieve_graph uri
          ("Unknown graph "^(Rdf_uri.string uri))
  in
  { default ; named = named_set ; get_named }
;;

let dataset ?get_named ?(named=Iriset.empty) default =
  match get_named with
    None -> simple_dataset default
  | Some get_named -> { default ; named ; get_named }
;;

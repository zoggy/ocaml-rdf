(** Dump in graphviz format. *)

open Rdf_node;;

module Node_set = Set.Make(Rdf_mem.Node_ord_type)

let dot_of_graph g =
  let b = Buffer.create 256 in
  Buffer.add_string b "digraph g {\nrankdir=LR;\nfontsize=10;\n";
  let triples = g.Rdf_graph.find () in
  let label node =
    match node with
      Uri uri -> Rdf_uri.string uri
    | Literal lit ->
        Printf.sprintf "%s%s%s" lit.lit_value
            (match lit.lit_language with None -> "" | Some s -> "^"^s)
            (match lit.lit_type with None -> "" | Some uri -> "@"^(Rdf_uri.string uri))
    | Blank_ _ | Blank -> ""
  in
  let id node =
    let s =
      match node with
        Uri uri -> Rdf_uri.string uri
      | Blank_ id -> "b"^(string_of_blank_id id)
      | Literal lit ->
          Printf.sprintf "%s^%s@%s" lit.lit_value
            (match lit.lit_language with None -> "" | Some s -> s)
            (match lit.lit_type with None -> "" | Some uri -> Rdf_uri.string uri)
      | Blank -> assert false
    in
    Printf.sprintf "N%s" (Digest.to_hex (Digest.string s))
  in
  let f set (sub, pred, obj) =
    Printf.bprintf b "%s -> %s [label=%S];\n" (id sub) (id obj) (label pred);
    Node_set.add sub (Node_set.add obj set)
  in
  let set = List.fold_left f Node_set.empty triples in
  let f_node node =
    Printf.bprintf b "%s [ label=%S ];\n" (id node) (label node)
  in
  Node_set.iter f_node set;
  Buffer.add_string b "}\n";
  Buffer.contents b
;;
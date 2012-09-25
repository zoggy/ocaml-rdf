(** Dump in graphviz format. *)

open Rdf_node;;

module Node_set = Set.Make
  (struct type t = Rdf_node.node
     let compare node1 node2 =
       match node1, node2 with
         Uri uri1, Uri uri2 -> Rdf_uri.compare uri1 uri2
       | Uri _, _ -> 1
       | _, Uri _ -> -1
       | Literal lit1, Literal lit2 ->
           begin
             match Pervasives.compare lit1.lit_value lit2.lit_value with
               0 ->
                 begin
                   match Pervasives.compare lit1.lit_language lit2.lit_language with
                     0 ->
                       begin
                         match lit1.lit_type, lit2.lit_type with
                           None, None -> 0
                         | None, _ -> 1
                         | _, None -> -1
                         | Some uri1, Some uri2 -> Rdf_uri.compare uri1 uri2
                       end
                   | n -> n
                 end
             | n -> n
           end
       | Literal _, _ -> 1
       | _, Literal _ -> -1
       | Blank, Blank -> 0
       | Blank, _ -> 1
       | _, Blank -> -1
       | Blank_ id1, Blank_ id2 ->
           Pervasives.compare
           (Rdf_node.string_of_blank_id id1)
           (Rdf_node.string_of_blank_id id2)
   end)

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
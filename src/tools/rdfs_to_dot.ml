(** *)
let get_by_type g t =
  let q =
   "PREFIX rdf: <"^(Rdf_iri.string Rdf_rdf.rdf)^">
    PREFIX rdfs: <"^(Rdf_iri.string Rdf_rdfs.rdfs)^">
    SELECT ?x
      { ?x a <"^(Rdf_iri.string t)^"> . }"
  in
  let ds = Rdf_ds.simple_dataset g in
  let q = Rdf_sparql.query_from_string q in
  let sols = Rdf_sparql.select (g.Rdf_graph.name()) ds q in
  let f acc sol =
    let prop = Rdf_sparql.get_iri sol (g.Rdf_graph.name()) "x" in
    prop :: acc
  in
  List.fold_left f [] sols
;;

let get_props g iri =
  let q =
   "PREFIX rdf: <"^(Rdf_iri.string Rdf_rdf.rdf)^">
    PREFIX rdfs: <"^(Rdf_iri.string Rdf_rdfs.rdfs)^">
    SELECT ?prop ?range
      { ?prop rdfs:domain <"^(Rdf_iri.string iri)^"> .
        ?prop rdfs:range ?range .
      }"
  in
  let ds = Rdf_ds.simple_dataset g in
  let q = Rdf_sparql.query_from_string q in
  let sols = Rdf_sparql.select (g.Rdf_graph.name()) ds q in
  let f acc sol =
    let prop = Rdf_sparql.get_iri sol (g.Rdf_graph.name()) "prop" in
    let range = Rdf_sparql.get_iri sol (g.Rdf_graph.name()) "range" in
    (prop, range) :: acc
  in
  List.fold_left f [] sols
;;

let get_parent_classes g iri =
  let q =
   "PREFIX rdf: <"^(Rdf_iri.string Rdf_rdf.rdf)^">
    PREFIX rdfs: <"^(Rdf_iri.string Rdf_rdfs.rdfs)^">
    SELECT ?cl
      { <"^(Rdf_iri.string iri)^"> rdfs:subClassOf ?cl .
      }"
  in
  let ds = Rdf_ds.simple_dataset g in
  let q = Rdf_sparql.query_from_string q in
  let sols = Rdf_sparql.select (g.Rdf_graph.name()) ds q in
  let f acc sol =
    let cl = Rdf_sparql.get_iri sol (g.Rdf_graph.name()) "cl" in
    cl :: acc
  in
  List.fold_left f [] sols
;;

let id =
  let cpt = ref 0 in
  let map = ref Rdf_iri.Irimap.empty in
  fun iri ->
    try Rdf_iri.Irimap.find iri !map
    with Not_found ->
      incr cpt ;
      let cpt = string_of_int !cpt in
      map := Rdf_iri.Irimap.add iri cpt !map;
      cpt
;;

let node iri = "N"^(id iri);;

let p = Printf.bprintf;;

let label ns iri =
  match Rdf_dot.apply_namespaces ns (Rdf_iri.string iri) with
    ("",s) -> s
  | (p,s) -> p^":"^s
;;

let print_class g b ns iri =
  let clabel = label ns iri in
  p b "%s [ label=< <TABLE><TR><TD PORT=\"P%s\">%s</TD></TR>" (node iri) (id iri) clabel;

  let f (prop, range) =
    Buffer.add_string b
     ("<TR><TD PORT=\"P"^(id prop)^"\">"^(label ns prop)^" : "^(label ns range)^"</TD></TR>");

    if Rdf_iri.equal range Rdf_rdfs.rdfs_Literal then
      None
    else
      (
       let s = (node iri)^":P"^(id prop)^" -> "^(node range)^":P"^(id range)^";\n" in
       Some s
      )
  in
  let edges = List.map f (get_props g iri) in
  p b "</TABLE> > ];\n";
  List.iter (function None -> () | Some s -> Buffer.add_string b s) edges;

  let f_subclass cl =
    p b "%s -> %s:P%s [ label=\"subClassOf\" ];\n" (node iri) (node cl) (id cl)
  in
  List.iter f_subclass (get_parent_classes g iri)
;;

let generate g =
  let g2 = get_props g in
  let ns = Rdf_dot.build_namespaces g in

  let b = Buffer.create 1024 in
  let p = Printf.bprintf b in
  p "digraph g {\n  rankdir=\"LR\";\n";
  p "  node [style=\"rounded,filled\", shape=\"rect\", color=\"red\", fillcolor=\"lightgrey\", fontcolor=\"black\"];\n";
  List.iter (print_class g b ns) (get_by_type g Rdf_rdfs.rdfs_Class);
  p "}";
  let dot = Buffer.contents b in
  print_endline dot
;;


let fatal s = prerr_endline s ; exit 1 ;;
let usage = Printf.sprintf "Usage: %s [options] <file>" Sys.argv.(0);;

let main () =
  let load = ref Rdf_xml.from_file in
  let read_base = ref None in
  let args = ref [] in
  Arg.parse
    [
      "--ttl", Arg.Unit (fun () -> load := Rdf_ttl.from_file),
      " indicate input file is in turtle format rather than XML/RDF" ;

      "-b", Arg.String (fun s -> read_base := Some s),
      " <iri> use <iri> as base used when reading graph, default is <base_iri>" ;
    ]
    (fun s -> args := s :: !args)
    (usage^"\nwhere options are:");
  match List.rev !args with
  | file :: _ ->
      begin
        try
          let base_iri = Rdf_iri.iri "http://foo.net" in
          let options = [ "storage", "mem" ] in
          let g = Rdf_graph.open_graph ~options base_iri in
          let base = match !read_base with
              None -> base_iri
            | Some s -> Rdf_iri.iri s
          in
          !load g ~base file ;
          generate g
        with
          Rdf_iri.Invalid_iri (s, msg) -> failwith ("Invalid IRI "^s^" : "^msg)
        | Rdf_ttl.Error e -> failwith (Rdf_ttl.string_of_error e)
        | Rdf_xml.Invalid_rdf msg -> failwith msg
        | Rdf_sparql.Error e -> failwith (Rdf_sparql.string_of_error e)
      end
  | _ -> fatal usage
;;


(*c==v=[Misc.safe_main]=1.0====*)
let safe_main main =
  try main ()
  with
    Failure s
  | Sys_error s ->
      prerr_endline s;
      exit 1
(*/c==v=[Misc.safe_main]=1.0====*)

let () = safe_main main;;
(** Running Turtle tests.
  @see http://www.w3.org/2011/rdf-wg/wiki/Turtle_Test_Suite
*)

let base = Rdf_uri.uri "http://www.w3.org/2013/TurtleTests/";;

let rdfs_uri = Rdf_uri.uri "http://www.w3.org/2000/01/rdf-schema#";;
let rdfs_ = Rdf_uri.append rdfs_uri;;

let mf_uri = Rdf_uri.uri "http://www.w3.org/2001/sw/DataAccess/tests/test-manifest#";;
let mf_ = Rdf_uri.append mf_uri ;;

let manifest = mf_"Manifest";;
let entries = mf_"entries";;
let action = mf_"action";;
let result = mf_"result";;

let qt_uri = Rdf_uri.uri "http://www.w3.org/2001/sw/DataAccess/tests/test-query#";;
let qt_ = Rdf_uri.append qt_uri ;;

let rdft_uri = Rdf_uri.uri "http://www.w3.org/ns/rdftest#";;
let rdft_ = Rdf_uri.append rdft_uri;;

let test_eval = rdft_"TestTurtleEval";;
let test_eval_neg = rdft_"TestTurtleNegativeEval";;
let test_stx_pos = rdft_"TestTurtlePositiveSyntax";;
let test_stx_neg = rdft_"TestTurtleNegativeSyntax";;

let namespaces =
  [ "rdf", Rdf_rdf.rdf_"" ;
    "rdfs", rdfs_uri ;
    "mf", mf_uri ;
    "qt", qt_uri ;
    "rdft", rdft_uri ;
  ];;

type result = Ok of Rdf_graph.graph | Error of string;;

type test =
  | EvalPos of Rdf_uri.uri
  | EvalNeg
  | SyntaxPos
  | SyntaxNeg
;;

let exec_select g q =
  let ds = Rdf_ds.simple_dataset g in
  let q =
    List.fold_right
      (fun (name, uri) acc ->
        "PREFIX "^name^": <"^(Rdf_uri.string uri)^">\n"^acc)
       namespaces
       q
  in
  try
    let q = Rdf_sparql.parse_from_string q in
    Rdf_sparql.select ~base ds q
  with
    Rdf_sparql.Error e ->
      failwith (q^"\n"^(Rdf_sparql.string_of_error e))
;;

let tests g =
  let q = "SELECT ?test ?action ?result ?type
           WHERE { ?test mf:action ?action .
                   ?test a ?type .
                   OPTIONAL { ?test mf:result ?result . }
                 }"
  in
  let f acc sol =
    let t = Rdf_sparql.get_iri sol base "type" in
    let typ =
      match t with
      | _ when Rdf_uri.compare t test_eval = 0 ->
          EvalPos (Rdf_sparql.get_iri sol base "result")
      | _ when Rdf_uri.compare t test_eval_neg = 0 ->
          EvalNeg
      | _ when Rdf_uri.compare t test_stx_pos = 0 ->
          SyntaxPos
      | _ when Rdf_uri.compare t test_stx_neg = 0 ->
          SyntaxNeg
      | _ -> failwith ("Unknown test type "^(Rdf_uri.string t))
    in
    (Rdf_sparql.get_iri sol base "test",
     Rdf_sparql.get_iri sol base "action",
     typ
    ) :: acc
  in
  List.fold_left f [] (exec_select g q)
;;


let map_blanks map g =
  let map_term = function
    Rdf_term.Blank_ s ->
      Rdf_term.Blank_ (Rdf_term.blank_id_of_string (Rdf_types.SMap.find (Rdf_term.string_of_blank_id s) map))
  | t -> t
  in
  let f (sub, pred, obj) =
    g.Rdf_graph.rem_triple ~sub ~pred ~obj ;
    let sub = map_term sub in
    let obj = map_term obj in
    g.Rdf_graph.add_triple ~sub ~pred ~obj
  in
  List.iter f (g.Rdf_graph.find ())
;;

open Rdf_term;;

let align_score mapped (sub, pred, obj) =
  let is_mapped b = Rdf_types.SSet.mem (Rdf_term.string_of_blank_id b) mapped in
  match sub, obj with
    Blank_ b1, Blank_ b2 ->
      (
       match is_mapped b1, is_mapped b2 with
         false, false -> 3
       | true, false -> 2
       | false, true -> 1
       | true, true -> 0
      )
  | Blank_ b, _ -> if is_mapped b then 2 else 0
  | _, Blank_ b -> if is_mapped b then 1 else 0
  | _ -> 0
;;


let sort_triples_for_align =
  let comp (s1, (_,p1,_)) (s2, (_,p2,_)) =
    match s1 - s2 with
      0 -> Rdf_uri.compare p1 p2
    | n -> n
  in
  fun mapped l ->
    let l = List.map (fun t -> (align_score mapped t, t)) l in
    let l = List.sort comp l in
    List.map snd l
;;

let triples_differ (s1, p1, o1) (s2, p2, o2) =
  let msg = "Triples differ:\n  "^
    (Rdf_ttl.string_of_triple ~sub: s1 ~pred: p1 ~obj: o1)^"\n  "^
      (Rdf_ttl.string_of_triple ~sub: s2 ~pred: p2 ~obj: o2)
  in
  failwith msg
;;

let add_binding bindings mapped b1 b2 =
  let s1 = Rdf_term.string_of_blank_id b1 in
  let s2 = Rdf_term.string_of_blank_id b2 in
  match String.compare s1 s2 with
    0 -> (bindings, mapped)
  | _ ->
      try
        let mapped_s1 = Rdf_types.SMap.find s1 bindings in
        if mapped_s1 = s2 then
          (bindings, mapped)
        else
          failwith ("bindings differ")
      with
        Not_found ->
          let bindings = Rdf_types.SMap.add s1 s2 bindings in
          let mapped = Rdf_types.SSet.add s2 mapped in
          (bindings, mapped)
;;

let bind bindings mapped ((s1, p1, o1) as t1) ((s2, p2, o2) as t2) =
  match Rdf_uri.compare p1 p2 with
    n when n <> 0 -> triples_differ t1 t2
  | _ ->
      let (bindings, mapped) =
        match s1, s2 with
          Blank_ b1, Blank_ b2 -> add_binding bindings mapped b1 b2
        | Blank_ _, _
        | _, Blank_ _ -> triples_differ t1 t2
        | _, _ -> (bindings, mapped)
      in
      match o1, o2 with
        Blank_ b1, Blank_ b2 -> add_binding bindings mapped b1 b2
      | Blank_ _, _
      | _, Blank_ _ -> triples_differ t1 t2
      | _, _ -> (bindings, mapped)
;;

let map_triple bindings (sub,pred,obj) =
  let map_term = function
    Blank_ b ->
      (
       try Blank_
          (Rdf_term.blank_id_of_string (Rdf_types.SMap.find (Rdf_term.string_of_blank_id b) bindings))
       with Not_found -> Blank_ b
      )
  | x -> x
  in
  let sub = map_term sub in
  let obj = map_term obj in
  (sub, pred, obj)
;;

let make_blank_map g1 g2 =
  let t1 = g1.Rdf_graph.find () in
  let t2 = g2.Rdf_graph.find () in
  let t1 = sort_triples_for_align Rdf_types.SSet.empty t1 in
  let t2 = sort_triples_for_align Rdf_types.SSet.empty t2 in
  let rec iter bindings mapped = function
    [], [] -> bindings
  | [], _
  | _, [] -> failwith "Graphs don't have the same number of triples"
  | t1 :: q1, t2 :: q2 ->
      let (bindings, mapped) = bind bindings mapped t1 t2 in
      let q1 = List.map (map_triple bindings) q1 in
      let q1 = sort_triples_for_align mapped q1 in
      let q2 = sort_triples_for_align mapped q2 in
      iter bindings mapped (q1, q2)
  in
  iter Rdf_types.SMap.empty Rdf_types.SSet.empty (t1, t2)
;;

let isomorph_graphs g1 g2 =
  try
    let map = make_blank_map g1 g2 in
    map_blanks map g1;

    let included g1 g2 =
      let f (sub, pred, obj) =
        match g2.Rdf_graph.find ~sub ~pred ~obj () with
          [] ->
            prerr_endline ("Triple not found: "^(Rdf_ttl.string_of_triple ~sub ~pred ~obj));
            g2.Rdf_graph.set_namespaces [];
            prerr_endline (Rdf_ttl.to_string g2);
            false
        | _ -> true
      in
      List.for_all f (g1.Rdf_graph.find ())
    in
    included g1 g2 && included g2 g1
  with
    Failure msg ->
      prerr_endline msg ;
      false
;;

let run_test (test, action, typ) =
  let in_file =
    match List.rev (Rdf_uri.path action) with
      file :: _ -> file
    | [] -> assert false
  in
  let result =
    try
      let g = Rdf_graph.open_graph action in
      ignore(Rdf_ttl.from_file g ~base: action in_file);
      Ok g
    with
      e ->
        let msg_e =
          match e with
            Rdf_ttl.Error e -> Rdf_ttl.string_of_error e
          | Rdf_uri.Invalid_url s -> "Invalid_url: "^s
          | e -> raise e
        in
        let msg = (Rdf_uri.string test) ^ "\n" ^ msg_e in
        Error msg
  in
  match result, typ with
    Error msg, EvalNeg
  | Error msg, SyntaxNeg ->
      prerr_endline ("OK "^(Rdf_uri.string test))
  | Error msg, _ ->
      prerr_endline ("*** KO "^(Rdf_uri.string test)^":\n"^msg)
  | Ok _, SyntaxNeg
  | Ok _, EvalNeg ->
      prerr_endline ("*** KO "^(Rdf_uri.string test))
  | Ok g, SyntaxPos ->
      prerr_endline ("OK "^(Rdf_uri.string test))
  | Ok g, EvalPos result ->
      let res_file =
        match List.rev (Rdf_uri.path result) with
          file :: _ -> file
        | [] -> assert false
      in
      let gres = Rdf_graph.open_graph action in
      ignore(Rdf_ttl.from_file gres ~base: action res_file) ;
      if isomorph_graphs g gres then
        prerr_endline ("OK "^(Rdf_uri.string test))
      else
        (
         let file = (Filename.chop_extension in_file)^".out" in
         g.Rdf_graph.set_namespaces [];
         Rdf_ttl.to_file g file;

         let file2 = (Filename.chop_extension in_file)^".out2" in
         gres.Rdf_graph.set_namespaces [];
         Rdf_ttl.to_file gres file2;

         prerr_endline ("*** KO "^(Rdf_uri.string test))
        )
;;

let run_tests g =
  let tests = tests g in
  prerr_endline (Printf.sprintf "%d tests read" (List.length tests));
  List.iter run_test tests
;;


let fatal s = prerr_endline s ; exit 1 ;;
let usage = Printf.sprintf "Usage: %s [options] files.ttl\nwhere options are:" Sys.argv.(0);;

let load_manifest g file =
  try ignore(Rdf_ttl.from_file g ~base file)
  with Rdf_ttl.Error e ->
      prerr_endline ("File "^file^": "^(Rdf_ttl.string_of_error e));
      exit 1
;;

let main () =
  let args = ref [] in
  Arg.parse []
    (fun s -> args := s :: !args)
    usage;

  match List.rev !args with
    [] -> fatal usage
  | file :: _ ->
      let options = [ "storage", "mem" ] in
      let g = Rdf_graph.open_graph ~options base in
      load_manifest g file ;
      run_tests g
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

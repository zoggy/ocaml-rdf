(** Running Turtle tests.
  @see http://www.w3.org/2011/rdf-wg/wiki/Turtle_Test_Suite
*)

let base = Rdf_iri.iri "http://www.w3.org/2013/TurtleTests/";;

let mf_iri = Rdf_iri.iri "http://www.w3.org/2001/sw/DataAccess/tests/test-manifest#";;
let mf_ = Rdf_iri.append mf_iri ;;

let manifest = mf_"Manifest";;
let entries = mf_"entries";;
let action = mf_"action";;
let result = mf_"result";;

let qt_iri = Rdf_iri.iri "http://www.w3.org/2001/sw/DataAccess/tests/test-query#";;
let qt_ = Rdf_iri.append qt_iri ;;

let rdft_iri = Rdf_iri.iri "http://www.w3.org/ns/rdftest#";;
let rdft_ = Rdf_iri.append rdft_iri;;

let test_eval = rdft_"TestTurtleEval";;
let test_eval_neg = rdft_"TestTurtleNegativeEval";;
let test_stx_pos = rdft_"TestTurtlePositiveSyntax";;
let test_stx_neg = rdft_"TestTurtleNegativeSyntax";;

let namespaces =
  [ "rdf", Rdf_rdf.rdf ;
    "rdfs", Rdf_rdfs.rdfs ;
    "mf", mf_iri ;
    "qt", qt_iri ;
    "rdft", rdft_iri ;
  ];;

type result = Ok of Rdf_graph.graph | Error of string;;

type test =
  | EvalPos of Rdf_iri.iri
  | EvalNeg
  | SyntaxPos
  | SyntaxNeg
;;

let exec_select g q =
  let ds = Rdf_ds.simple_dataset g in
  let q =
    List.fold_right
      (fun (name, iri) acc ->
        "PREFIX "^name^": <"^(Rdf_iri.string iri)^">\n"^acc)
       namespaces
       q
  in
  try
    let q = Rdf_sparql.query_from_string q in
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
      | _ when Rdf_iri.compare t test_eval = 0 ->
          EvalPos (Rdf_sparql.get_iri sol base "result")
      | _ when Rdf_iri.compare t test_eval_neg = 0 ->
          EvalNeg
      | _ when Rdf_iri.compare t test_stx_pos = 0 ->
          SyntaxPos
      | _ when Rdf_iri.compare t test_stx_neg = 0 ->
          SyntaxNeg
      | _ -> failwith ("Unknown test type "^(Rdf_iri.string t))
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

type align_form =
  ATT of term * Rdf_iri.iri * term
| ABT of string * Rdf_iri.iri * term
| ATB of term * Rdf_iri.iri * string
| ABB of string * Rdf_iri.iri * string
;;

let align_form mapped (sub, pred, obj) =
  let is_mapped b = Rdf_types.SSet.mem b mapped in
  match sub, obj with
    Blank_ b1, Blank_ b2 ->
      let s1 = Rdf_term.string_of_blank_id b1 in
      let s2 = Rdf_term.string_of_blank_id b2 in
      (
       match is_mapped s1, is_mapped s2 with
         false, false -> ABB (s1, pred, s2)
       | true, false -> ATB (sub, pred, s2)
       | false, true -> ABT (s1, pred, obj)
       | true, true -> ATT (sub, pred, obj)
      )
  | Blank_ b, _ ->
      let s = Rdf_term.string_of_blank_id b in
      if is_mapped s then
        ATT (sub, pred, obj)
      else
        ABT (s, pred, obj)
  | _, Blank_ b ->
      let s = Rdf_term.string_of_blank_id b in
      if is_mapped s then
        ATT (sub, pred, obj)
      else
        ATB (sub, pred, s)
  | _ ->
      ATT (sub, pred, obj)
;;


let sort_triples_for_align =
  let comp (f1, _) (f2, _) =
    match f1, f2 with
      ATT (s1, p1, o1), ATT (s2, p2, o2) ->
        begin
          match Rdf_iri.compare p1 p2 with
            0 ->
              (match Rdf_term.compare s1 s2 with
                 0 -> Rdf_term.compare o1 o2
               | n -> n
              )
          | n -> n
        end
    | ATT _, _ -> 1
    | _, ATT _ -> -1
    | ATB (s1, p1, o1), ATB (s2, p2, o2) ->
        begin
          match Rdf_iri.compare p1 p2 with
            0 ->
              (match Rdf_term.compare s1 s2 with
                 0 -> String.compare o1 o2
               | n -> n
              )
          | n -> n
        end
    | ATB _, _ -> 1
    | _, ATB _ -> -1
    | ABT (s1, p1, o1), ABT (s2, p2, o2) ->
        begin
          match Rdf_iri.compare p1 p2 with
            0 ->
              (match Rdf_term.compare o1 o2 with
                 0 -> String.compare s1 s2
               | n -> n
              )
          | n -> n
        end
    | ABT _, _ -> 1
    | _, ABT _ -> -1
    | ABB (s1,p1,o1), ABB (s2,p2,o2) ->
        begin
          match Rdf_iri.compare p1 p2 with
            0 ->
              (match String.compare s1 s2 with
                 0 -> String.compare o1 o2
               | n -> n
              )
          | n -> n
        end
  in
  fun mapped l ->
    let l = List.map (fun t -> (align_form mapped t, t)) l in
    let l = List.sort comp l in
    (* rev_map because we sorted with blanks first, because
      of the use of Rdf_term.compare *)
    List.rev_map snd l
;;

let triples_differ (s1, p1, o1) (s2, p2, o2) rest1 rest2 =
  let b = Buffer.create 256 in
  Buffer.add_string b "Triples differ:\n  ";
  Buffer.add_string b (Rdf_ttl.string_of_triple ~sub: s1 ~pred: p1 ~obj: o1);
  Buffer.add_string b "\n  ";
  Buffer.add_string b (Rdf_ttl.string_of_triple ~sub: s2 ~pred: p2 ~obj: o2);
  Buffer.add_string b "\n  Rest1:";
  List.iter
    (fun (sub,pred,obj) ->
       Buffer.add_string b "\n  ";
       Buffer.add_string b (Rdf_ttl.string_of_triple ~sub ~pred ~obj)
    )
    rest1 ;
  Buffer.add_string b "\n  Rest2:";
  List.iter
    (fun (sub,pred,obj) ->
       Buffer.add_string b "\n  ";
       Buffer.add_string b (Rdf_ttl.string_of_triple ~sub ~pred ~obj)
    )
    rest2 ;
  failwith (Buffer.contents b)
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

let bind bindings mapped ((s1, p1, o1) as t1) ((s2, p2, o2) as t2) q1 q2 =
  match Rdf_iri.compare p1 p2 with
    n when n <> 0 -> triples_differ t1 t2 q1 q2
  | _ ->
      let (bindings, mapped) =
        match s1, s2 with
          Blank_ b1, Blank_ b2 -> add_binding bindings mapped b1 b2
        | Blank_ _, _
        | _, Blank_ _ -> triples_differ t1 t2 q1 q2
        | _, _ -> (bindings, mapped)
      in
      match o1, o2 with
        Blank_ b1, Blank_ b2 -> add_binding bindings mapped b1 b2
      | Blank_ _, _
      | _, Blank_ _ -> triples_differ t1 t2 q1 q2
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
  (*prerr_endline "make_blank_map";
  prerr_endline " sorted triples1:";
  List.iter (fun (sub,pred,obj) -> prerr_endline ("  "^(Rdf_ttl.string_of_triple ~sub ~pred ~obj))) t1 ;
  prerr_endline " sorted triples2:";
  List.iter (fun (sub,pred,obj) -> prerr_endline ("  "^(Rdf_ttl.string_of_triple ~sub ~pred ~obj))) t2 ;
  *)
  let rec iter bindings mapped = function
    [], [] -> bindings
  | [], _
  | _, [] -> failwith "Graphs don't have the same number of triples"
  | t1 :: q1, t2 :: q2 ->
      let (bindings, mapped) = bind bindings mapped t1 t2 q1 q2 in
      let q1 = List.map (map_triple bindings) q1 in
      let q1 = sort_triples_for_align mapped q1 in
      let q2 = sort_triples_for_align mapped q2 in
      iter bindings mapped (q1, q2)
  in
  iter Rdf_types.SMap.empty Rdf_types.SSet.empty (t1, t2)
;;

let isomorphic_graphs g1 g2 =
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
    match List.rev (Rdf_iri.path action) with
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
          | Rdf_iri.Invalid_iri (s,msg) -> "Invalid iri "^s^" : "^msg
          | e -> raise e
        in
        let msg = in_file ^ "\n" ^ msg_e in
        Error msg
  in
  match result, typ with
    Error msg, EvalNeg
  | Error msg, SyntaxNeg ->
      prerr_endline ("OK "^(Rdf_iri.string test))
  | Error msg, _ ->
      prerr_endline ("*** KO "^(Rdf_iri.string test)^":\n"^msg)
  | Ok _, SyntaxNeg
  | Ok _, EvalNeg ->
      prerr_endline ("*** KO (success instead of error)"^(Rdf_iri.string test))
  | Ok g, SyntaxPos ->
      prerr_endline ("OK "^(Rdf_iri.string test))
  | Ok g, EvalPos result ->
      let res_file =
        match List.rev (Rdf_iri.path result) with
          file :: _ -> file
        | [] -> assert false
      in
      let gres = Rdf_graph.open_graph action in
      ignore(Rdf_ttl.from_file gres ~base: action res_file) ;
      if isomorphic_graphs g gres then
        prerr_endline ("OK "^(Rdf_iri.string test))
      else
        (
         let file = (Filename.chop_extension in_file)^".out" in
         g.Rdf_graph.set_namespaces [];
         Rdf_ttl.to_file g file;

         let file2 = (Filename.chop_extension in_file)^".out2" in
         gres.Rdf_graph.set_namespaces [];
         Rdf_ttl.to_file gres file2;

         prerr_endline ("*** KO "^(Rdf_iri.string test))
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

(** Running Turtle tests.
  @see http://www.w3.org/2011/rdf-wg/wiki/Turtle_Test_Suite
*)

let base = Rdf_uri.uri "http://example/base/";;

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


let run_test (test, action, typ) =
  let in_file =
    match List.rev (Rdf_uri.path action) with
      file :: _ -> file
    | [] -> assert false
  in
(*
  let res_file =
    match result with
      None -> None
    | Some u ->
        match List.rev (Rdf_uri.path u) with
          file :: _ -> Some file
        | [] -> assert false
  in
*)
  let result =
    try
      let g = Rdf_graph.open_graph base in
      ignore(Rdf_ttl.from_file g base in_file);
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
  | Ok _, SyntaxNeg ->
      prerr_endline ("*** KO "^(Rdf_uri.string test))
  | Ok g, _ ->
      prerr_endline ("OK "^(Rdf_uri.string test))
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

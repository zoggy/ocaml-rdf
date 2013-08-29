(** Main module of the rdf_sparql_bench tool. *)

open Rdf_sparql_test;;
open Rdf_sparql;;

let base = Rdf_uri.uri "http://localhost/ocamml-rdf-bench";;
let prop_ = Rdf_uri.concat base;;

let prop_options = prop_"graphOptions";;
let prop_source = prop_"source";;
let prop_query = prop_"query";;
let prop_duration = prop_"duration";;
let prop_datasize = prop_"dataSize";;
let prop_result = prop_"result";;
let prop_error = prop_"error";;
let type_op_sparql = prop_"sparqlQuery";;
let type_op_import = prop_"import";;

let get_time () = Unix.gettimeofday ();;

let result_file = ref "benchmark_results.ttl";;
let option_result =
  ("--result", Arg.Set_string result_file,
   "<file> append results in graph of <file>; default is "^ !result_file
  )
;;

type operation_kind = Import of result | Sparql_query of result
type operation =
  {
    datasize : int ;
    kind : operation_kind ;
    duration : float option ;
    spec : test_spec ;
  }

let store_operation g op =
  let b = Rdf_term.Blank_ (g.Rdf_graph.new_blank_id ()) in
  let add ~pred ~obj = g.Rdf_graph.add_triple ~sub: b ~pred ~obj in

  let options_string =
    let options = List.filter
      (function
         ("name",_) | ("database",_) | ("user",_)
       | ("password",_) | ("host",_) | ("port",_) -> false
       | _ -> true
      )
      op.spec.options
    in
    String.concat " ; "
      (
       List.map (fun (o, v) -> (String.escaped o)^"="^(String.escaped v))
         options
      )
  in
  add ~pred: prop_options ~obj: (Rdf_term.term_of_literal_string options_string);
  (match op.duration with
    None -> ()
  | Some d -> add ~pred: prop_duration ~obj: (Rdf_term.term_of_double d)
  );

  add ~pred: prop_datasize ~obj: (Rdf_term.term_of_int op.datasize);

  let res =
    match op.kind, op.spec.default_graph with
      Import _, None -> assert false
    | Import res, Some file ->
        add ~pred: Rdf_rdf.rdf_type ~obj: (Rdf_term.Uri type_op_import);
        add ~pred: prop_source ~obj: (Rdf_term.term_of_literal_string file);
        res
    | Sparql_query res, _ ->
        add ~pred: Rdf_rdf.rdf_type ~obj: (Rdf_term.Uri type_op_sparql);
        add ~pred: prop_query
          ~obj: (Rdf_term.term_of_literal_string (Rdf_misc.string_of_file op.spec.query));
        let source =
          match op.spec.default_graph with
          | Some file -> Rdf_term.term_of_literal_string file
          | None ->
              Rdf_term.Uri
                (match op.spec.base with
                   None -> Rdf_uri.uri "http://localhost/"
                 | Some u -> u)
        in
        add ~pred: prop_source ~obj: source;
        res
  in
  match res with
          Rdf_sparql_test.Error s ->
            add ~pred: prop_error ~obj: (Rdf_term.term_of_literal_string s)
        | Ok x ->
            let obj =
              match x with
                Bool b -> Rdf_term.term_of_bool b
              | Solutions l -> Rdf_term.term_of_int (List.length l)
              | Graph g ->
                  let file = Filename.temp_file ~temp_dir: (Sys.getcwd()) "result" ".ttl" in
                  Rdf_ttl.to_file g file ;
                  Rdf_term.term_of_literal_string file
            in
            add ~pred: prop_result ~obj
;;

let run_sparql_test spec =
  let (duration, res, size) =
    try
      let dataset = Rdf_sparql_test.mk_dataset spec in
      let query = Rdf_sparql.parse_from_file spec.query in
      let t_start = get_time () in
      let base = match spec.base with None -> Rdf_uri.uri "http://localhost/" | Some u -> u in
      let res = Rdf_sparql.execute base dataset query in
      let t_stop = get_time () in
      let duration = t_stop -. t_start in
      let size = dataset.Rdf_ds.default.Rdf_graph.size () in
      (Some duration, Ok res, size)
  with
    e ->
      let msg =
        match e with
          Rdf_sparql.Error e -> Rdf_sparql.string_of_error e
        | Rdf_ttl.Error e ->
            Rdf_ttl.string_of_error e
        | _ -> raise e
      in
      (None, Rdf_sparql_test.Error msg, 0)
  in
  let op = {
      kind = Sparql_query res ; datasize = size ;
      spec ; duration ;
    }
  in
  let g = Rdf_graph.open_graph base in
  store_operation g op ;
  g
;;

let run_import_test spec =
  let spec_base =
    match spec.base with
      None -> Rdf_uri.uri "http://localhost/"
    | Some uri -> uri
  in
  let g = Rdf_graph.open_graph ~options: spec.options spec_base in
  if g.Rdf_graph.size () > 0 then
    prerr_endline ("Warning: graph "^(Rdf_uri.string spec_base)^" not empty");
  let (res, duration, size) =
    try
      (* use a in-memory graph first, so that parsing time
         is not counted, then merge the loaded graph into g *)
      let g0 =
        match spec.default_graph with
          None -> failwith "No default graph to import"
        | Some file ->
            let g = Rdf_graph.open_graph spec_base in
            ignore(Rdf_ttl.from_file g file);
            g
      in
      let t_start = get_time () in
      Rdf_graph.merge g g0;
      let t_stop = get_time () in
      let duration = t_stop -. t_start in
      let size = g0.Rdf_graph.size () in
      (Ok (Bool true), Some duration, size)
    with
      e ->
        let msg =
          match e with
          | Rdf_ttl.Error e ->
              Rdf_ttl.string_of_error e
          | _ -> raise e
        in
        (Rdf_sparql_test.Error msg, None, 0)
  in
  let op = {
      kind = Import res ; datasize = size ;
      spec ; duration ;
    }
  in
  let g = Rdf_graph.open_graph base in
  store_operation g op ;
  g
;;

type mode = Import | Sparql
let mode = ref Sparql;;

let options = [
    option_result ;
    "--import", Arg.Unit (fun () -> mode := Import),
    " perform an import test instead of a sparql test" ;
  ]
;;

let usage_string = Printf.sprintf
  "Usage: %s [options] <test files>\nwhere options are:" Sys.argv.(0);;

let main () =
  let files = ref [] in
  Arg.parse options (fun f -> files := f :: !files) usage_string;
  match List.rev !files with
    [] -> prerr_endline (Arg.usage_string options usage_string); exit 1
  | files ->
      let specs = List.map Rdf_sparql_test.load_file files in
      let run_test =
        match !mode with
          Sparql -> run_sparql_test
        | Import -> run_import_test
      in
      let graphs = List.map run_test specs in
      let g = Rdf_graph.open_graph base in
      if Sys.file_exists !result_file then
        ignore(Rdf_ttl.from_file g ~base !result_file);
      List.iter (Rdf_graph.merge g) graphs;
      Rdf_ttl.to_file g !result_file
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

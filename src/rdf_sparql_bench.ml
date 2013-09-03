(** Main module of the rdf_sparql_bench tool. *)

open Rdf_sparql_test;;
open Rdf_sparql;;


let base = Rdf_uri.uri "http://localhost/ocamml-rdf-bench";;
let prop_ = Rdf_uri.concat base;;

let prop_options = prop_"graphOptions";;
let prop_source = prop_"source";;
let prop_query = prop_"query";;
let prop_query_file = prop_"queryFile";;
let prop_duration = prop_"duration";;
let prop_datasize = prop_"dataSize";;
let prop_result = prop_"result";;
let prop_error = prop_"error";;
let prop_id = prop_"id";;
let type_op_sparql = prop_"sparqlQuery";;
let type_op_import = prop_"import";;

let get_time () = Unix.gettimeofday ();;


type operation_kind = Import of result | Sparql_query of result
type operation =
  {
    datasize : int ;
    kind : operation_kind ;
    duration : float option ;
    spec : test_spec ;
    id : string ;
  }

let store_operation g op =
  let b = Rdf_term.Blank_ (g.Rdf_graph.new_blank_id ()) in
  let add ~pred ~obj = g.Rdf_graph.add_triple ~sub: b ~pred ~obj in

  let options_string =
    let options = List.filter
      (function
         ("name",_) | ("database",_) | ("user",_)
       | ("password",_) | ("host",_) | ("port",_) | ("id",_) -> false
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

  add ~pred: prop_id ~obj: (Rdf_term.term_of_literal_string op.id);

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
        add ~pred: prop_query_file
          ~obj: (Rdf_term.term_of_literal_string op.spec.query);
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
      prerr_endline s;
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

let run_sparql_test ~id spec =
  let (duration, res, size) =
    try
      let dataset = Rdf_sparql_test.mk_dataset spec in
      let query = Rdf_sparql.parse_from_file spec.query in
      let base = match spec.base with None -> Rdf_uri.uri "http://localhost/" | Some u -> u in
      prerr_endline "Running sparql query";
      let t_start = get_time () in
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
      spec ; duration ; id ;
    }
  in
  let g = Rdf_graph.open_graph base in
  store_operation g op ;
  g
;;

let run_import_test ~id spec =
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
            prerr_endline ("loading graph from "^file);
            ignore(Rdf_ttl.from_file g spec_base file);
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
      spec ; duration ; id ;
    }
  in
  let g = Rdf_graph.open_graph base in
  store_operation g op ;
  g
;;
let sparql_select g q =
  (*prerr_endline q;*)
  let q = Rdf_sparql.parse_from_string q in
  let dataset = Rdf_ds.simple_dataset g in
  Rdf_sparql.select ~base dataset q
;;

module SMap = Rdf_xml.SMap;;
type import_stats = (int * float option SMap.t) list

let us = Rdf_uri.string ;;

let ids g =
  let q = "SELECT DISTINCT ?id
           WHERE { _:a <"^(us prop_id)^"> ?id. }
           ORDER BY DESC(?id)"
  in
  List.fold_left
    (fun acc sol ->
       let id = Rdf_sparql.get_string sol "id" in
       id :: acc
    ) []
    (sparql_select g q)
;;

let sizes_of_op g ?(clause="") op =
  let q =
    "SELECT DISTINCT ?size
     WHERE { ?run <"^(us prop_datasize)^"> ?size .
             ?run <"^(us Rdf_rdf.rdf_type)^"> <"^(us op)^"> .
             "^clause^"}
     ORDER BY DESC(?size)"
  in
  let sols = sparql_select g q in
  List.fold_left
    (fun acc sol -> Rdf_sparql.get_int sol "size" :: acc)
    [] sols
;;

let import_stats ids g =
  let sizes = sizes_of_op g type_op_import in
  let f_op size acc id =
    let term_id = Rdf_term.term_of_literal_string id in
    let q = "SELECT (AVG(?dur) as ?duration)
      WHERE { _:run <"^(us prop_id)^"> "^(Rdf_term.string_of_term term_id)^".
              _:run <"^(us prop_datasize)^"> "^(string_of_int size)^" .
              _:run <"^(us Rdf_rdf.rdf_type)^"> <"^(us type_op_import)^"> .
              _:run <"^(us prop_duration)^"> ?dur .
              FILTER (?dur > 0.0)
            }"
    in
    match sparql_select g q with
      [] -> SMap.add id None acc
    | [sol] ->
        begin
          try SMap.add id (Some (get_float sol "duration")) acc
          with _ -> SMap.add id None acc
        end
    | _ -> assert false
  in
  let f_size size =
    (size, List.fold_left (f_op size) SMap.empty ids)
  in
  List.map f_size sizes
;;

let queries g =
  let q = "SELECT DISTINCT ?qfile ?query
           WHERE { _:a <"^(us prop_query)^"> ?query.
                   _:a <"^(us prop_query_file)^"> ?qfile .
                 }
           ORDER BY DESC(?qfile)"
  in
  List.fold_left
    (fun acc sol ->
       let qfile = Rdf_sparql.get_string sol "qfile" in
       let query = Rdf_sparql.get_string sol "query" in
       (qfile, query) :: acc
    ) []
    (sparql_select g q)
;;

type sparql_stat =
  { query_file : string ;
    query : string ;
    backend_durations : (int * float option SMap.t) list ; (* size * (duration by id) *)
  }

let sparql_stat ids g (qfile, query) =
  let term_file = Rdf_term.term_of_literal_string qfile in
  let clause =
    "_:run <"^(us prop_query_file)^"> "^(Rdf_term.string_of_term term_file)^"."
  in
  let sizes = sizes_of_op g ~clause type_op_sparql in
  let f_id size map id =
    let term_id = Rdf_term.term_of_literal_string id in
    let q = "SELECT (AVG(?dur) as ?duration)
       WHERE { _:run <"^(us prop_id)^"> "^(Rdf_term.string_of_term term_id)^".
              _:run <"^(us prop_datasize)^"> "^(string_of_int size)^" .
              _:run <"^(us Rdf_rdf.rdf_type)^"> <"^(us type_op_sparql)^"> .
              "^clause^"
              _:run <"^(us prop_duration)^"> ?dur .
              FILTER (?dur > 0.0)
            }"
    in
    match sparql_select g q with
      [] -> SMap.add id None map
    | [sol] ->
        begin
          try SMap.add id (Some (get_float sol "duration")) map
          with _ -> SMap.add id None map
        end
    | _ -> assert false
  in
  let f_size size =
    let map = List.fold_left (f_id size) SMap.empty ids in
    (size, map)
  in
  let backend_durations = List.map f_size sizes in
   { query_file = qfile ; query ; backend_durations }
;;

let sparql_stats ids g =
  let queries = queries g in
  List.map (sparql_stat ids g) queries
;;

let print_duration_table p pn ids str rows =
  (* ensure ids are in the same order as in duration maps *)
  let ids = List.fold_left (fun map id -> SMap.add id 0 map) SMap.empty ids in
  pn "<table class=\"table table-bordered table-condensed table-bench\">";
  p "<thead><th>Nb. of triples</th>";
  SMap.iter (fun s _ -> p ("<th>"^s^"</th>")) ids;
  pn "</thead>";
  let f_row (size, map) =
    p "<tr><td><strong>";
    p (string_of_int size);
    p "</strong></td>";
    SMap.iter
      (fun _ dopt ->
         p "<td>";
         p (match dopt with None -> " " | Some d -> str d);
         p "</td>"
      )
      map;
    pn "</tr>";
  in
  List.iter f_row rows;
  pn "</table>";
;;

let report g outfile =
  let oc = open_out outfile in
  let p s = output_string oc s in
  let pn s = output_string oc s; output_string oc "\n" in
  pn "<page\ntitle=\"Benchmarks\"\n>";
  pn "<prepare-toc><toc/>";
  pn "<p>Execution times are in seconds. Executions were run on
    a personal machine (8 cores, Intel 1.87GHz, 64 bits debian).
    </p><p>Mysql version: 5.5.31</p><p>Postgresql version: 9.3</p>";

  let ids = ids g in

  pn "<section id=\"import\" title=\"Importing triples\">";
  print_duration_table p pn ids
   (fun d -> Printf.sprintf "%.2f" d)
   (import_stats ids g);
  pn "</section>";

  pn "<section id=\"sparql\" title=\"Executing sparql queries\">";
  let f_sparql t =
    let file = Filename.basename t.query_file in
    pn ("<subsection id=\""^file^"\" title=\""^file^"\">");
    pn ("<hcode lang=\"sql\"><![CDATA["^t.query^"]]></hcode>");
    print_duration_table p pn ids
      (fun d -> Printf.sprintf "%.4f" d)
      t.backend_durations ;
    pn "</subsection>";
  in
  List.iter f_sparql (sparql_stats ids g);
  pn "</section>";

  pn "</prepare-toc>";
  pn "</page>";
  close_out oc
;;

type mode = Import | Sparql | Html of string
let mode = ref Sparql;;

let graph_options = ref None;;

let result_file = ref "benchmark_results.ttl";;
let id = ref None;;

let options = [
    "--id", Arg.String (fun s -> id := Some s),
    "id associate id to this run";

    "--result", Arg.Set_string result_file,
    "file append results in graph of <file>; default is "^ !result_file ;

    "--import", Arg.Unit (fun () -> mode := Import),
    " perform an import test instead of a sparql test" ;

    "--goptions", Arg.String (fun s -> graph_options := Some s),
    "s override the graph_options of the spec files." ;

    "--html", Arg.String (fun s -> mode := Html s),
    "file read benchmark graph and output stog page to <file>" ;
  ]
;;

let usage_string = Printf.sprintf
  "Usage: %s [options] [<test files>]\nwhere options are:" Sys.argv.(0);;

let main () =
  let files = ref [] in
  Arg.parse options (fun f -> files := f :: !files) usage_string;
  match !mode with
    Html file ->
      begin
        try
          let g = Rdf_graph.open_graph base in
          ignore(Rdf_ttl.from_file g ~base !result_file);
          report g file
        with
        Rdf_sparql.Error e ->
            prerr_endline (Rdf_sparql.string_of_error e);
            exit 1
      end
  | _ ->
      match List.rev !files with
        [] -> prerr_endline (Arg.usage_string options usage_string); exit 1
      | files ->
          match !id with
            None -> failwith "No id provided. Please use --id option."
          | Some id ->
              let specs = List.map
                (Rdf_sparql_test.load_file ?graph_options: !graph_options)
                  files
              in
              let run_test =
                match !mode with
                  Sparql -> run_sparql_test
                | Import -> run_import_test
                | Html _ -> assert false
              in
              let graphs = List.map (run_test ~id) specs in
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

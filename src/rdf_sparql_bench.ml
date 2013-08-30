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
    id : string option ;
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

  (match op.id with
    None -> ()
  | Some id -> add ~pred: prop_id ~obj: (Rdf_term.term_of_literal_string id)
  );

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

let run_sparql_test ?id spec =
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
      spec ; duration ; id ;
    }
  in
  let g = Rdf_graph.open_graph base in
  store_operation g op ;
  g
;;

let run_import_test ?id spec =
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
  let q = Rdf_sparql.parse_from_string q in
  let dataset = Rdf_ds.simple_dataset g in
  Rdf_sparql.select ~base dataset q
;;

module SMap = Rdf_xml.SMap;;
type import_stats = (int * float option SMap.t) list

let us = Rdf_uri.string ;;

let options_ids g =
  let q = "SELECT DISTINCT ?id
           WHERE { _:a <"^(us prop_id)^"> ?id. }
           ORDER BY DESC(?id)"
  in
  List.fold_left
    (fun acc sol ->
       let term = Rdf_sparql.get_term sol "id" in
       match Rdf_dt.string (Rdf_dt.of_term term) with
         Rdf_dt.String s -> (s, term) :: acc
       | _ -> acc
    ) []
    (sparql_select g q)
;;

let import_stats g =
  let qsizes =
    "SELECT DISTINCT ?size
     WHERE { ?run <"^(us prop_datasize)^"> ?size .
             ?run <"^(us Rdf_rdf.rdf_type)^"> <"^(us type_op_import)^"> .}
     ORDER BY DESC(?size)"
  in
  let sols = sparql_select g qsizes in
  let f_op n acc (s, term) =
    let q = "SELECT (AVG(?dur) as ?duration)
      WHERE { _:run <"^(us prop_id)^"> "^(Rdf_term.string_of_term term)^".
              _:run <"^(us prop_datasize)^"> "^(string_of_int n)^" .
              _:run <"^(us Rdf_rdf.rdf_type)^"> <"^(us type_op_import)^"> .
              _:run <"^(us prop_duration)^"> ?dur .
              FILTER (?dur > 0.0)
            }"
    in
    match sparql_select g q with
      [] -> SMap.add s None acc
    | [sol] ->
        begin
          match Rdf_dt.float (Rdf_dt.of_term (Rdf_sparql.get_term sol "duration")) with
            Rdf_dt.Float d -> SMap.add s (Some d) acc
          | _ -> SMap.add s None acc
        end
    | _ -> assert false
  in
  let options_ids = options_ids g in
  let f_size n =
    (n, List.fold_left (f_op n) SMap.empty options_ids)
  in
  let f_size acc sol =
    match Rdf_dt.int (Rdf_dt.of_term (Rdf_sparql.get_term sol "size")) with
      Rdf_dt.Int n -> (f_size n) :: acc
    | _ -> acc
  in
  List.fold_left f_size [] sols

;;

let report g outfile =
  let oc = open_out outfile in
  let p s = output_string oc s in
  let pn s = output_string oc s; output_string oc "\n" in
  pn "<page\ntitle=\"Benchmarks\"\n>";
  pn "<prepare-toc><toc/>";
  pn "<section id=\"import\" title=\"Importing triples\">";

  let ids = options_ids g in
  pn "<table>";
  p "<tr><td>Size</td>";
  List.iter (fun (s,_) -> p ("<td>"^s^"</td>")) ids;
  pn "</tr>";

  let f_import (size, map) =
    p "<tr><td><strong>";
    p (string_of_int size);
    p "</strong></td>";
    SMap.iter
      (fun _ dopt ->
        p "<td>";
        p (match dopt with None -> " " | Some d -> string_of_float d);
        p "</td>"
      )
      map;
    pn "</tr>"
  in
  List.iter f_import (import_stats g);
  pn "</table>";
  pn "</section>";

  pn "<section id=\"sparql\" title=\"Executing sparql queries\">";

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
          let id = !id in
          let graphs = List.map (run_test ?id) specs in
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

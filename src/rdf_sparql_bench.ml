(** Main module of the rdf_sparql_bench tool. *)

open Rdf_sparql_test;;

let base = Rdf_uri.uri "http://localhost/ocamml-rdf-bench/";;
let prop_ = Rdf_uri.concat base;;

let prop_options = prop_"graphOptions";;
let prop_source = prop_"source";;
let prop_query = prop_"query";;
let prop_duration = prop_"duration";;
let prop_datasize = prop_"dataSize";;
let prop_result = prop_"result";;
let prop_error = prop_"error";;

let result_file = ref "benchmark_results.ttl";;
let option_result =
  ("--result", Arg.Set_string result_file,
   "<file> append results in graph of <file>; default is "^ !result_file
  )
;;


let run_test spec =
  try
    let dataset = Rdf_sparql_test.mk_dataset spec in
    let query = Rdf_sparql.parse_from_file spec.query in
    let res = Rdf_sparql.execute spec.base dataset query in
    { spec ; result = Ok res }
  with
    e ->
      let msg =
        match e with
          Rdf_sparql.Error e -> Rdf_sparql.string_of_error e
        | Rdf_ttl.Error e ->
            Rdf_ttl.string_of_error e
        | _ -> raise e
      in
      { spec ; result = Error msg }
;;

let options = [ option_result ];;
let usage_string = Printf.sprintf "Usage: %s [options] <test files>\nwhere options are:" Sys.argv.(0);;

let main () =
  let files = ref [] in
  Arg.parse options (fun f -> files := f :: !files) usage_string;
  match List.rev !files with
    [] -> prerr_endline (Arg.usage_string options usage_string); exit 1
  | files ->
      let specs = List.map Rdf_sparql_test.load_file files in
      let tests = List.map run_test specs in
      List.iter (fun t -> print_result t.result) tests ;
      ()

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

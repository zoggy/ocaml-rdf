(** Main module of the rdf_sparql_test tool. *)

open Rdf_sparql_test;;

let run_test spec =
  try
    let dataset = mk_dataset spec in
    let query = Rdf_sparql.query_from_file spec.query in
    let base = match spec.base with
      None -> Rdf_iri.iri "http://foo.bar"
    | Some iri -> iri
    in
    let res = Rdf_sparql.execute base dataset query in
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

let options = [];;
let usage_string = Printf.sprintf "Usage: %s [options] <test files>\nwhere options are:" Sys.argv.(0);;

let main () =
  let files = ref [] in
  Arg.parse options (fun f -> files := f :: !files) usage_string;
  match List.rev !files with
    [] -> prerr_endline (Arg.usage_string options usage_string); exit 1
  | files ->
    let specs = List.map load_file files in
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
(** Testing. *)

module C = Config_file;;

open Rdf_sparql_types;;

let verb = print_endline;;

type test_spec =
  { base : Rdf_uri.uri ;
    title : string ;
    desc : string option;
    query : string ;
    default_graph : string option ;
    named : (Rdf_uri.uri * string) list ;
  }

type result = Error of string | Ok of Rdf_sparql_ms.mu list

type test = {
  spec : test_spec ;
  result : result ;
  }

let load_file file =
  verb ("Loading file "^file);
  let group = new C.group in
  let base = new C.string_cp ~group ["base"] "http://localhost/" "Base uri" in
  let title = new C.string_cp ~group ["title"] "" "Title of the test" in
  let desc = new C.string_cp ~group ["descr"] "" "Description of the test" in
  let query = new C.string_cp ~group ["query"] "" "File containing the query" in
  let default_graph = new C.string_cp ~group ["default_graph"] "" "File containing the default graph" in
  let named = new C.list_cp (C.tuple2_wrappers C.string_wrappers C.string_wrappers) ~group
    [ "named_graphs"] [] "Named graphs in the form (uri, file.ttl)"
  in
  group#read file;
  let mk_filename =
    let path = Filename.dirname file in
    fun file ->
      if Filename.is_relative file then
        Filename.concat path file
      else
        file
  in
  let named = List.map (fun (uri, s) -> (Rdf_uri.uri uri, mk_filename s)) named#get in
  { base = Rdf_uri.uri base#get ;
    title = title#get ;
    desc = Rdf_misc.opt_of_string desc#get ;
    query = mk_filename query#get ;
    default_graph = Rdf_misc.opt_of_string default_graph#get ;
    named ;
  }
;;

let load_ttl g base file =
  verb ("Loading graph from "^file);
  try Rdf_ttl.from_file g ~base file
  with
  | Rdf_ttl.Error e ->
      prerr_endline (Rdf_ttl.string_of_error e);
      exit 1
;;

let mk_dataset spec =
  let default =
    let g = Rdf_graph.open_graph spec.base in
    match spec.default_graph with
       None -> g
     | Some file -> load_ttl g spec.base file
  in
  let f (uri, file) =
    let g = Rdf_graph.open_graph spec.base in
    let g = load_ttl g spec.base file in
    (uri, g)
  in
  let named = List.map f spec.named in
  Rdf_ds.simple_dataset ~named default
;;

let print_solution mu =
  Rdf_sparql_ms.SMap.iter
    (fun name term -> print_string (name^"->"^(Rdf_node.string_of_node term)^" ; "))
    mu.Rdf_sparql_ms.mu_bindings;
  print_newline()
;;
let print_result = function
  Error s -> print_endline ("ERROR: "^s)
| Ok solutions ->
   Printf.printf "%d Solution(s):\n" (List.length solutions);
   List.iter print_solution solutions
;;

let run_test spec =
  try
    let dataset = mk_dataset spec in
    let query = Rdf_sparql.parse_from_file spec.query in

    let (base, ds, query) = Rdf_sparql_expand.expand_query spec.base query in
    let q =
      match query.q_kind with
        Select s ->
          { Rdf_sparql_algebra.query_proj = Some s.select_select ;
            query_where = s.select_where ;
            query_modifier = s.select_modifier ;
            query_values = None ;
          }
      | _ -> failwith "only select queries implemented"
    in
    let algebra = Rdf_sparql_algebra.translate_query_level q in
    print_endline (Rdf_sparql_algebra.string_of_algebra algebra);
    print_endline (Rdf_ttl.to_string dataset.Rdf_ds.default);
    let ctx = Rdf_sparql_eval.context ~base
      ?from: ds.Rdf_sparql_expand.from
        ~from_named: ds.Rdf_sparql_expand.from_named dataset
    in
    let omega = Rdf_sparql_eval.eval_list ctx algebra in
    { spec ; result = Ok omega }
  with
    e ->
      let msg =
        match e with
          Rdf_sparql.Error e -> Rdf_sparql.string_of_error e
        | Rdf_sparql_eval.Unknown_fun iri ->
            "Unknown function "^(Rdf_uri.string iri)
        | Rdf_sparql_eval.Unbound_variable v ->
            Printf.sprintf "%sUnbound variable %S"
              (Rdf_loc.string_of_loc v.var_loc) v.var_name
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
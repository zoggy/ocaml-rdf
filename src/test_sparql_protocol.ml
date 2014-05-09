(** Testing Sparql protocol HTTP binding. *)

open Rdf_sparql_protocol;;
open Rdf_sparql;;

let fatal s = prerr_endline s ; exit 1 ;;
let usage = Printf.sprintf "Usage: %s <url> <query>" Sys.argv.(0);;

let (>>=) t f = Lwt.bind t f

(*c==v=[Misc.safe_main]=1.0====*)
let safe_main main =
  try main ()
  with
    Failure s
  | Sys_error s ->
      prerr_endline s;
      exit 1
(*/c==v=[Misc.safe_main]=1.0====*)

let main () =
  let query_file = ref None in
  let accept = ref None in
  let post = ref false in
  let remain = ref [] in
  let options =
    [ "--accept", Arg.String (fun s -> accept := Some s),
      "<s> set the Accept header used in HTTP request";
      
      "--post", Arg.Set post, " send POST request instead of GET" ;
      
      "--query-file", Arg.String (fun s -> query_file := Some s),
      "<file> read query from file instead of second argument" ;
    ]
  in
  Arg.parse options (fun f -> remain := !remain @ [f]) usage;
  match !remain with
    [] -> fatal usage
  | url :: q ->
      let query =
        match !query_file, q with
          Some f, _ -> Rdf_misc.string_of_file f
        | None, query :: _ -> query
        | None, [] -> fatal usage
      in
      let options = [ "storage", "mem" ] in
      let base = Rdf_iri.iri "http://hello.fr" in
      let g = Rdf_graph.open_graph ~options base in
      let url = Rdf_uri.uri url in
      let in_message = { in_query = query ; in_dataset = empty_dataset } in
      let f =
        match !post with
          false -> Rdf_sparql_http_lwt.get
         | true -> Rdf_sparql_http_lwt.post
      in
      f ~graph: g ~base ?accept: !accept url in_message >>=
        (fun res ->
           let () =
             match res with
               Rdf_sparql_protocol.Error e -> prerr_endline (Rdf_sparql_protocol.string_of_error e)
             | Ok -> print_endline "Ok"
             | Result res ->
                 match res with
             | Rdf_sparql.Bool true -> print_endline "true"
                 | Rdf_sparql.Bool false -> print_endline "false"
                 | Rdf_sparql.Graph _ -> 
                     let file = "/tmp/"^(Filename.basename Sys.argv.(0))^".ttl" in
                     Rdf_ttl.to_file g file;
                     print_endline ("graph stored in "^file)
                 | Rdf_sparql.Solutions sols ->
                     let f_sol sol =
                       Rdf_sparql.solution_iter
                         (fun name term -> print_string (name^"->"^(Rdf_term.string_of_term term)^" ; "))
                         sol;
                       print_newline()
                     in
                     print_endline "Solutions:";
                     List.iter f_sol sols
           in
           Lwt.return_unit
        )
;;


let () =
  try Lwt_main.run (safe_main main)
  with Rdf_sparql_http.Invalid_response (s1, s2) ->
    prerr_endline ("Invalid response: "^s1^"\n"^s2);
    exit 1
;;

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
  if Array.length Sys.argv < 3 then fatal usage;
  let options = [ "storage", "mem" ] in
  let base = Rdf_iri.iri "http://hello.fr" in
  let g = Rdf_graph.open_graph ~options base in
  let url = Rdf_uri.uri Sys.argv.(1) in
  let query = Sys.argv.(2) in
  let in_message = { in_query = query ; in_dataset = empty_dataset } in
  Rdf_sparql_http_lwt.get ~graph: g ~base url in_message >>=
    (fun res ->
       let () =
         match res with
           Rdf_sparql_protocol.Error e -> prerr_endline (Rdf_sparql_protocol.string_of_error e)
         | Ok -> print_endline "Ok"
         | Result res ->
             match res with
             | Rdf_sparql.Bool true -> print_endline "true"
             | Rdf_sparql.Bool false -> print_endline "false"
             | Rdf_sparql.Graph _ -> print_endline "graph"
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


let () = Lwt_main.run (safe_main main);;
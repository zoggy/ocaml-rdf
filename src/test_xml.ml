(** Testing xml parser. *)

let fatal s = prerr_endline s ; exit 1 ;;
let usage = Printf.sprintf "Usage: %s file.rdf" Sys.argv.(0);;

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
  let file =
    if Array.length Sys.argv < 2 then fatal usage;
    Sys.argv.(1)
  in
  let options = [ "storage", "mem" ] in
  let g = Rdf_graph.open_graph ~options (Rdf_uri.uri "http://hello.fr") in
  Rdf_xml.from_file g (Rdf_uri.uri file) file;
  let dot = Rdf_dot.dot_of_graph g in
  print_string dot;
  Rdf_xml.to_file g "/tmp/foo.rdf"

;;


let () = safe_main main;;


    
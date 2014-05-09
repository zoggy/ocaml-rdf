
type 'a result =
 | Ok of 'a
 | Error of string

(*** Tools  ***)

(* Getting result *)

let result_of_response f (header, body) =
  let status = Cohttp.Code.code_of_status (Cohttp.Response.status header) in
  lwt body_string = Cohttp_lwt_body.to_string body in
  if (status >= 200 && status < 300) then
    try Lwt.return (Ok (f body_string))
    with e -> Lwt.return (Error (Printexc.to_string e))
  else
    Lwt.return (Error body_string)

(* Other tools *)

let base_headers () =
  let headers = Cohttp.Header.init_with "accept" "application/json" in
  Cohttp.Header.add headers "user-agent" ("ocaml-rdf/"^Rdf_config.version)

let clean_query query =
  let regexp = Str.regexp "[\n]+" in
  Str.global_replace regexp " " query

(*** Binding  ***)

let get uri ?default_graph_uri ?named_graph_uri query =
  let concat arg_name q uri = q ^ "&" ^ arg_name ^ "=" ^ (Rdf_uri.string uri) in
  let fold_left name value query_uri = match value with
    | None      -> query_uri
    | Some l    -> List.fold_left (concat name) query_uri l
  in
  let query_url =
    fold_left "named-graph-uri" named_graph_uri
      (fold_left "default-graph-uri" default_graph_uri
         ((Rdf_uri.string uri) ^ "?query=" ^ (clean_query query)))
  in
  print_endline query_url;
  let uri = Uri.of_string query_url in
  let headers = base_headers () in
  let get_result body_string =
    let body_assoc = Yojson.Basic.from_string body_string in
    Rdf_json.sparql_result body_assoc
  in
  lwt res = Cohttp_lwt_unix.Client.get ~headers uri in
  result_of_response get_result res

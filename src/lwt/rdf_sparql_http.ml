module Yojson = Yojson.Basic

type 'a result =
 | Ok of 'a
 | Error of string

(*** Tools  ***)

(* Result casting  *)

let mk_term v = function
  | "uri"                       -> Rdf_term.term_of_iri_string v
  | "literal"                   -> Rdf_term.(Literal (mk_literal v))
  | _ when String.length v != 0 -> Rdf_term.(Blank_ (blank_id_of_string v))
  | _                           -> Rdf_term.Blank

let term_of_json json =
  let e_type = Yojson.Util.(to_string (member "type" json)) in
  let value = Yojson.Util.(to_string (member "value" json)) in
  mk_term value e_type

let couple_of_json mu (name, j_term) =
  Rdf_sparql_ms.mu_add name (term_of_json j_term) mu

let solution_of_json json =
  Rdf_sparql.solution_of_mu
    (List.fold_left couple_of_json Rdf_sparql_ms.mu_0
       (Yojson.Util.to_assoc json))

let solutions_of_json json =
  List.map solution_of_json (Yojson.Util.to_list json)

let string_of_json string_list json =
  (Yojson.Util.to_string json)::string_list

let head_of_json json =
  List.fold_left string_of_json [] (Yojson.Util.to_list json)

let get_solutions body_string =
  let body_assoc = Yojson.from_string body_string in
  let results_assoc = Yojson.Util.(member "results" body_assoc) in
  let bindings = Yojson.Util.(member "bindings" results_assoc) in
  solutions_of_json bindings

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
  Cohttp.Header.add headers "user-agent" "ocaml-rdf/0.8"

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
         ((Rdf_uri.string uri) ^ "/sparql/?query=" ^ (clean_query query)))
  in
  print_endline query_url;
  let uri = Uri.of_string query_url in
  let headers = base_headers () in
  lwt res = Cohttp_lwt_unix.Client.get ~headers uri in
  result_of_response get_solutions res

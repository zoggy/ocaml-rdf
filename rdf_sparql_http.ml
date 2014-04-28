module Yj = Yojson.Safe

exception Invalid_result

(*** Tools  ***)

(* Results casting  *)

let mk_term v = function
  | "uri"                       -> Rdf_term.term_of_iri_string v
  | "literal"                   -> Rdf_term.Literal (Rdf_term.mk_literal v)
  | _ when String.length v != 0 -> Rdf_term.Blank_ (Rdf_term.blank_id_of_string v)
  | _                           -> Rdf_term.Blank

let term_of_json = function
  | `Assoc ["type", `String e_type; "value", `String value]
        -> mk_term value e_type
  | _   -> raise Invalid_result

let solution_of_json = function
  | `Assoc [s_name, subject; p_name, predicate; o_name, s_object] ->
    Rdf_sparql_ms.(mu_add s_name (term_of_json subject)
                     (mu_add p_name (term_of_json predicate)
                        (mu o_name (term_of_json s_object))))
  | _   -> raise Invalid_result

let solutions_of_json = function
  | `List json  -> List.map solution_of_json json
  | `Null       -> []
  | _           -> raise Invalid_result

let head_of_json = function
  | `List [`String s; `String p; `String o]     -> [s; p; o]
  | `List []                                    -> []
  | `Null                                       -> []
  | _                                           -> raise Invalid_result

let get_solutions body =
  lwt body_string = Cohttp_lwt_body.to_string body in
  let json = Yj.from_string body_string in
  match json with
  | `Assoc ["head", `Assoc ["vars", head];
            "results", `Assoc ["bindings", results]]
        -> Lwt.return (head_of_json head, solutions_of_json results)
  | _   -> raise Invalid_result

(* Other tools *)

let base_headers () =
  let headers = Cohttp.Header.init_with "accept" "application/json" in
  Cohttp.Header.add headers "user-agent" "ocaml-rdf/0.8"

let solutions_of_response (header, body) =
  lwt _, solutions = get_solutions body in
  Lwt.return (header, solutions)

(*** Binding  ***)

let get url ?default_graph_uri ?named_graph_uri query =
  let concat arg_name q v = q ^ "&" ^ arg_name ^ "=" ^ v in
  let fold_left name value query_url = match value with
    | None      -> query_url
    | Some l    -> List.fold_left (concat name) query_url l
  in
  let query_url =
    fold_left "named-graph-uri" named_graph_uri
      (fold_left "default-graph-uri" default_graph_uri
         (url ^ "/sparql/?query=" ^ query))
  in
  let uri = Uri.of_string query_url in
  let headers = base_headers () in
  lwt res = Cohttp_lwt_unix.Client.get ~headers uri in
  solutions_of_response res

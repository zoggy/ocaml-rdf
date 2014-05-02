module Yj = Yojson.Safe

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

let term_of_json = function
  | `Assoc ["type", `String e_type; "value", `String value]
        -> mk_term value e_type
  | _   -> failwith "Invalid term result"

let couple_of_json mu = function
  | name, j_term      -> Rdf_sparql_ms.mu_add name (term_of_json j_term) mu
  | _                 -> failwith "Invalid couple result"

let solution_of_json = function
  | `Assoc l    -> Rdf_sparql.solution_of_mu
    (List.fold_left couple_of_json Rdf_sparql_ms.mu_0 l)
  | _           -> failwith "Invalid couple list result"

let solutions_of_json = function
  | `List json  -> List.map solution_of_json json
  | `Null       -> []
  | _           -> failwith "Invalid bindings result"

let string_of_json l = function
  | `String s   -> s::l
  | _           -> failwith "Invalid body `String missing"

let head_of_json = function
  | `List l     -> List.fold_left string_of_json [] l
  | `Null       -> []
  | _           -> failwith "Invalid header `List missing"

(** get the value from the given yojson assoc *)
let get_assoc = function
  | `Assoc yj   -> yj
  | _           -> failwith "Invalidy body, `Assoc missing"

(** Get the first value of the given name from a yojson list  *)
let rec get_from_list name = function
  | (n, v)::tail when n = name  -> v
  | _::tail                     -> get_from_list name tail
  | []                          -> failwith ("Invalid body: '" ^ name ^ "' not found")

let get_solutions body_string =
  let body_assoc = Yj.from_string body_string in
  let results_assoc = get_from_list "results" (get_assoc body_assoc) in
  let bindings = get_from_list "bindings" (get_assoc results_assoc) in
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
  Str.global_replace regexp "" query

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


(** Tools  *)

let default_type = "x-turtle"

let get_headers ?content_type ?content_length () =
  let map f value header = match value with
    | None      -> header
    | Some v    -> f header v
  in
  let add arg_name f hd v = Cohttp.Header.add hd arg_name (f v) in
  map (add "content-length" string_of_int) content_length
    (map (add "content-type" ((^) "application/")) content_type
       (Rdf_sparql_http_lwt.base_headers ()))

let body_of_string body_string =
  let body_stream = Cohttp_lwt_body.create_stream
    (fun s -> Lwt.return (Cohttp.Transfer.Final_chunk s)) body_string
  in
  Cohttp_lwt_body.of_stream body_stream

let result_of_null_response = Rdf_sparql_http_lwt.result_of_response (fun _ -> ())

(** Binding  *)

let get = Rdf_sparql_http_lwt.get

let post_update uri query =
  let uri = Uri.of_string ((Rdf_uri.string uri) ^ "/update/") in
  let body_string = Uri.pct_encode ("update=" ^ (Rdf_sparql_http_lwt.clean_query query)) in
  let content_length = String.length body_string in
  let body = body_of_string body_string in
  let headers = get_headers ~content_type:"x-www-form-urlencoded"
    ~content_length ()
  in
  lwt response = Cohttp_lwt_unix.Client.post ~body ~chunked:false ~headers uri in
  result_of_null_response response

let delete uri graph_uri =
  let uri = Uri.of_string
    ((Rdf_uri.string uri) ^ "/data/?graph=" ^ (Rdf_uri.string graph_uri))
  in
  let headers = get_headers () in
  lwt response = Cohttp_lwt_unix.Client.delete ~headers uri in
  result_of_null_response response

let put uri data ?(data_type=default_type) graph_uri =
  let uri = Uri.of_string
    ((Rdf_uri.string uri) ^ "/data/" ^ (Rdf_uri.string graph_uri))
  in
  let content_length = String.length data in
  let body = body_of_string data in
  let headers = get_headers ~content_type:data_type ~content_length () in
  lwt response = Cohttp_lwt_unix.Client.put ~body ~chunked:false ~headers uri in
  result_of_null_response response

let post_append uri data ?(data_type=default_type) graph_uri =
  let uri = Uri.of_string ((Rdf_uri.string uri) ^ "/data/") in
  let body_string = Uri.pct_encode
    ("data=" ^ data ^ "&graph=" ^ (Rdf_uri.string graph_uri) ^
        "&mime-type=application/" ^ data_type)
  in
  let content_length = String.length body_string in
  let body = body_of_string body_string in
  let headers = get_headers ~content_type:"x-www-form-urlencoded"
    ~content_length ()
  in
  lwt response = Cohttp_lwt_unix.Client.post ~body ~chunked:false ~headers uri in
  result_of_null_response response

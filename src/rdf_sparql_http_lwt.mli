(** Lwt-based Sparql PRotocol HTTP Binding.

   Using the {!Rdf_sparql_http.Make} functor.
*)


(** [base_headers ()] Gives the base headers used for bindings *)
val base_headers : ?accept: string -> unit -> Cohttp.Header.t

(** [result_of_response f response]
    [f] will be applied on the content-type and body string of the response.
    If the status is between 200 and 300 exclued, [f] is called,
    else a {!Rdf_sparql_protocol.Error} is returned.
*)
val result_of_response:
  (content_type: string -> string -> Rdf_sparql_protocol.out_message) ->
    Cohttp.Response.t * Cohttp_lwt_body.t ->
    Rdf_sparql_protocol.out_message Lwt.t

include Rdf_sparql_http.S with type result = Rdf_sparql_protocol.out_message Lwt.t
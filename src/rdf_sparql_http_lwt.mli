(**
   This Module implements the http binding of the RDF Sparql http protocol.
*)

(** {6 Tools}  *)

(** [base_headers ()] Gives the base headers used for bindings *)
val base_headers : ?accept: string -> unit -> Cohttp.Header.t

(** [result_of_response f response]
    [f] will be applied on the body string of the response.
    If the status is between 200 and 300 exclued, [f] is called,
    else an Error is returned. *)
val result_of_response: (content_type: string -> string -> Rdf_sparql_protocol.out_message) ->
  Cohttp.Response.t * Cohttp_lwt_body.t ->
  Rdf_sparql_protocol.out_message Lwt.t

(** [clean_query query]
    Remove \n in the given query *)
val clean_query : string -> string

(** {6 Binding} *)

include Rdf_sparql_http.S with type result = Rdf_sparql_protocol.out_message Lwt.t
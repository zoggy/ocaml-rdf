(**
   {b Rdf Sparql Http -
   This Module implement the http binding of the RDF Sparql http protocol}
*)

type 'a result =
 | Ok of 'a
 | Error of string

(** {6 Tools}  *)

(** [base_headers ()] Gives the base headers used for bindings *)
val base_headers : unit -> Cohttp.Header.t

(** [result_of_response f response]
    [f] will be applied on the body string of the response.
    If the status is between 200 and 300 exclued, [f] is called,
    else an Error is returned. *)
val result_of_response: (string -> 'a) ->
  Cohttp.Response.t * Cohttp_lwt_body.t ->
  'a result Lwt.t

(** [clean_query query]
    Remove \n in the given query *)
val clean_query : string -> string

(** {6 Binding} *)

(** [get uri ?default_graph_uri ?named_graph_uri query]
    [uri] The server's url (including the port)
    [query] The graph selection have to be made in the query.
    This method allow select/ask/describe query. *)
val get :
  Rdf_uri.uri -> ?default_graph_uri:Rdf_uri.uri list ->
  ?named_graph_uri:Rdf_uri.uri list -> string ->
  Rdf_sparql.solution list result Lwt.t

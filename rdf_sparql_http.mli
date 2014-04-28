(**
   {b Rdf Sparql Http -
   This Module implement the http binding of the RDF Sparql http protocol}
*)

exception Invalid_result

(** {6 Tool}  *)

(** [get_results response]
    It return a tuple containing the head and solutions *)
val get_solutions : Cohttp_lwt_body.t ->
  (string list * Rdf_sparql_ms.mu list) Lwt.t

(** [base_headers ()] Gives the base headers used for bindings *)
val base_headers : unit -> Cohttp.Header.t

(** [solutions_of_response response]
    Convert response to the tuple header, solutions *)
val solutions_of_response :
  (Cohttp_lwt_unix.Client.Response.t * Cohttp_lwt_body.t) Lwt.t ->
  (Cohttp_lwt_unix.Client.Response.t * Rdf_sparql_ms.mu list) Lwt.t

(** {6 Binding}
    @raise exception Invalid_result if bad response format. *)

(** [get url ?default_graph_uri ?named_graph_uri query]
    [url] The server's url (including the port)
    [query] The graph selection have to be made in the query.
    This method allow select/ask/describe query.*)
val get :
  string -> ?default_graph_uri:string list ->
  ?named_graph_uri:string list -> string ->
  (Cohttp_lwt_unix.Client.Response.t * Rdf_sparql_ms.mu list) Lwt.t

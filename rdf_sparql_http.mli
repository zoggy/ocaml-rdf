(**
   {b Rdf Sparql Http -
   This Module implement the http binding of the RDF Sparql http protocol}
*)

exception Invalid_result

(** {6 Tool}  *)

(** [base_headers ()] Gives the base headers used for bindings *)
val base_headers : unit -> Cohttp.Header.t

(** {6 Binding} *)

(** [get url ?default_graph_uri ?named_graph_uri query]
    @raise exception Invalid_result if bad response format.
    [url] The server's url (including the port)
    [query] The graph selection have to be made in the query.
    This method allow select/ask/describe query.
    It returns a tuple header, solutions *)
val get :
  string -> ?default_graph_uri:string list ->
  ?named_graph_uri:string list -> string ->
  (Cohttp_lwt_unix.Client.Response.t * Rdf_sparql_ms.mu list) Lwt.t

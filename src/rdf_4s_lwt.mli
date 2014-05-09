(**
   {b Rdf 4Store -
   This Module implement the http binding of 4Store}
*)

open Rdf_sparql_http_lwt

(** {6 Bindings}
    In this document:
    [uri] is the server's uri (including the port)
    [query] is the SPARQL query
    [graph_uri] is the uri of the graph where the query is going to be executed.
    [graph_uri] if is equal to [uri] it is executed on the default graph
    [data_type] are set at "x-turtle" by default. *)

(** [get uri ?default_graph_uri ?named_graph_uri query]
    [query] If you could like to select a named-graph, it has to be made in the query.
    This method allows: select/ask/describe query.*)
val get : Rdf_uri.uri -> ?default_graph_uri:Rdf_uri.uri list ->
  ?named_graph_uri:Rdf_uri.uri list -> string ->
  Rdf_sparql.query_result result Lwt.t

(** [post_update uri query]
    [query] If you would like to update a named-graph, it has to be made in the query.
    This method allows: update/insert/delete query. *)
val post_update : Rdf_uri.uri -> string -> unit result Lwt.t

(** [delete uri graph_uri]
    This method removes the entire [graph_uri].*)
val delete : Rdf_uri.uri -> Rdf_uri.uri -> unit result Lwt.t

(** [put uri data ?data_type graph_uri]
    This method allows to replace initial data from [graph_uri] by [data].
    [data] is allowed as rdf-xml type.*)
val put : Rdf_uri.uri -> string -> ?data_type:string -> Rdf_uri.uri ->
  unit result Lwt.t

(** [post_append uri data ?data_type graph_uri]
    This method allow to append [data] into [graph_uri].
    [data] is allow as ttl type. *)
val post_append : Rdf_uri.uri -> string -> ?data_type:string -> Rdf_uri.uri ->
  unit result Lwt.t

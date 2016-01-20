(*********************************************************************************)
(*                OCaml-RDF                                                      *)
(*                                                                               *)
(*    Copyright (C) 2012-2014 Institut National de Recherche en Informatique     *)
(*    et en Automatique. All rights reserved.                                    *)
(*                                                                               *)
(*    This program is free software; you can redistribute it and/or modify       *)
(*    it under the terms of the GNU Lesser General Public License version        *)
(*    3 as published by the Free Software Foundation.                            *)
(*                                                                               *)
(*    This program is distributed in the hope that it will be useful,            *)
(*    but WITHOUT ANY WARRANTY; without even the implied warranty of             *)
(*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the              *)
(*    GNU General Public License for more details.                               *)
(*                                                                               *)
(*    You should have received a copy of the GNU General Public License          *)
(*    along with this program; if not, write to the Free Software                *)
(*    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA                   *)
(*    02111-1307  USA                                                            *)
(*                                                                               *)
(*    Contact: Maxence.Guesdon@inria.fr                                          *)
(*                                                                               *)
(*********************************************************************************)

(** HTTP bindings for Sparql protocol.

  [http://www.w3.org/TR/rdf-sparql-protocol/#query-bindings-http]

  Using [http://www.w3.org/2005/sparql-results#] as reference for
  representation of Sparql results.

  This module provides a functor to create a HTTP binding.
  It requires a module able to send GET and POST requests
  and handle the response. The encoding and decoding of the
  Sparql protocol into request contents is done by the functor.
*)

exception Unsupported_content_type of string (** content-type *)

exception Invalid_response of string * string (** error * body *)

(** The type of the module implementing HTTP requests. *)
module type P =
  sig
    type 'a t

    val get : Uri.t -> ?accept: string ->
      (content_type:string -> string -> Rdf_sparql_protocol.out_message) ->
        Rdf_sparql_protocol.out_message t

    val post : Uri.t ->
      ?accept: string -> content_type: string -> content: string ->
        (content_type: string -> string ->  Rdf_sparql_protocol.out_message) ->
        Rdf_sparql_protocol.out_message t
  end

(** The signature of the resulting module when buildting the binding. *)
module type S =
  sig
    type result

    (** [get uri msg] sends the Sparql query corresponding to the given
         {!Rdf_sparql_protocol.in_message} with the GET method.
         [uri] is the complete uri of the service to connect to,
         i.e. not only the protocol, host and port, but also the
         path of the service, for example [http://dbpedia.org/sparql].
         @param graph and [base] parameters are the same as in
         {!Rdf_sparql.execute}.
         @param accept can be used to specify the [Accept:] header. Beware
         that the implementation handles only some content-types. This
         parameter can be used to require a response in, for example,
         the "application/rdf+xml" format.
    *)
    val get : ?graph: Rdf_graph.graph -> base:Iri.t -> ?accept: string ->
      Uri.t -> Rdf_sparql_protocol.in_message -> result

    (** Same as {!get} but using the POST method. To be used when sending
      large queries.
        @param query_var allows to change the message query name send
        ("query" by default)
    *)
    val post : ?graph: Rdf_graph.graph -> base:Iri.t -> ?accept: string ->
      Uri.t -> ?query_var: string ->
      Rdf_sparql_protocol.in_message -> result
  end

module Make : functor (P : P) -> S with type result = Rdf_sparql_protocol.out_message P.t

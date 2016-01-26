(*********************************************************************************)
(*                OCaml-RDF                                                      *)
(*                                                                               *)
(*    Copyright (C) 2012-2016 Institut National de Recherche en Informatique     *)
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

open Rdf_sparql_protocol

(** Tools  *)

let get_headers ?content_type ?content_length () =
  let map f value header = match value with
    | None      -> header
    | Some v    -> f header v
  in
  let add arg_name f hd v = Cohttp.Header.add hd arg_name (f v) in
  map (add "content-length" string_of_int) content_length
    (map (add "content-type" (fun v -> v)) content_type
       (Rdf_sparql_http_lwt.base_headers ()))

let body_of_string body_string =
  let body_stream = Cohttp_lwt_body.create_stream
    (fun s -> Lwt.return (Cohttp.Transfer.Final_chunk s)) body_string
  in
  Cohttp_lwt_body.of_stream body_stream

let result_of_null_response = Rdf_sparql_http_lwt.result_of_response
  (fun ~content_type _ -> Rdf_sparql_protocol.Ok)

(** Binding  *)

let get = Rdf_sparql_http_lwt.get

let post_update ?graph ~base ?accept uri msg =
  Rdf_sparql_http_lwt.post ?graph ~base ?accept uri ~query_var:"update" msg

let delete uri graph_uri =
  let query =
    let k = Uri.pct_encode ~component: `Query_key "graph" in
    let v = Uri.to_string graph_uri in
    [ k, [v] ]
  in
  let uri' = Uri.with_query uri query in
  let headers = get_headers () in
  let%lwt response = Cohttp_lwt_unix.Client.delete ~headers uri' in
  result_of_null_response response

let put uri content content_type graph_uri =
  let uri = Uri.of_string ((Uri.to_string uri) ^ (Uri.to_string graph_uri)) in
  let content_length = String.length content in
  let body = body_of_string content in
  let headers = get_headers ~content_type ~content_length () in
  let%lwt response = Cohttp_lwt_unix.Client.put ~body ~chunked:false ~headers uri in
  result_of_null_response response

let post_append uri content content_type graph_uri =
  let content' = (content ^ "&graph=" ^ (Uri.to_string graph_uri) ^
                    "&mime-type=" ^ content_type)
  in
  let base = Iri.of_string "" in
  let msg = {in_query = content'; in_dataset = empty_dataset} in
  Rdf_sparql_http_lwt.post ~base uri ~query_var:"data" msg

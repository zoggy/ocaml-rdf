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

open Rdf_sparql_protocol

(* Getting result *)

let result_of_response f (header, body) =
  let status = Cohttp.Code.code_of_status (Cohttp.Response.status header) in
  let content_type =
    match Cohttp.Header.get (Cohttp.Response.headers header) "Content-Type"with
      None -> ""
    | Some s -> s
  in
  let%lwt body_string = Cohttp_lwt_body.to_string body in
  match status with
  | 400 ->  Lwt.return (Error (Malformed_query body_string))
  | 500 ->  Lwt.return (Error (Query_request_refused body_string))
  | n when n >= 200 && n < 300 ->
      Lwt.return (f ~content_type body_string)
  | n ->
      Lwt.return
        (Error
          (Error_other ("HTTP return code: "^(string_of_int n))))
;;

(* Other tools *)

let base_headers ?accept () =
  let headers =
    match accept with
      None -> Cohttp.Header.init ()
    | Some s -> Cohttp.Header.init_with "accept" s
  in
  Cohttp.Header.add headers "user-agent" ("ocaml-rdf/"^Rdf_config.version)


module P =
  struct
    type 'a t = 'a Lwt.t
    let get uri ?accept f =
      let uri = Uri.of_string (Rdf_uri.string uri) in
      let headers = base_headers ?accept () in
      let%lwt res = Cohttp_lwt_unix.Client.get ~headers uri in
      result_of_response f res

    let post (uri : Rdf_uri.uri) ?accept ~content_type ~content
      (f : content_type: string -> string -> out_message) =
      let uri = Uri.of_string (Rdf_uri.string uri) in
      let headers = base_headers ?accept () in
      let headers = Cohttp.Header.add headers "Content-Type" content_type in
      let headers = Cohttp.Header.add headers
        "Content-Length" (string_of_int (String.length content))
      in
      let body =
        let stream = Cohttp_lwt_body.create_stream
          (fun s -> Lwt.return (Cohttp.Transfer.Final_chunk s)) content
        in
        Cohttp_lwt_body.of_stream stream
      in
      let%lwt res = Cohttp_lwt_unix.Client.post ~body ~chunked: false ~headers uri in
      result_of_response f res

  end

module M = Rdf_sparql_http.Make (P)

include M
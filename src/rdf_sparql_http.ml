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

(** *)

open Rdf_sparql_protocol
open Rdf_xml

exception Unsupported_content_type of string
exception Invalid_response of string * string

(* sparql-results namespace *)
let spr_ns = "http://www.w3.org/2005/sparql-results#"


module Xml =
  struct
    let first_child xml tag =
      match xml with
        D _ -> None
      | E ((_,_),subs) ->
          try Some (List.find (function E (((_,t),_),_) -> t = tag | _ -> false) subs)
      with Not_found -> None

    let get_att name atts =
     try Some (List.assoc name atts)
      with Not_found -> None

    let get_binding_name = get_att ("","name")

    let term_of_xmls =
      let rec iter = function
      | [] -> None
      | (D _) :: q -> iter q
      | (E (((_,"uri"),_), [D s])) ::_ ->
          Some (Rdf_term.term_of_iri_string s)
      | (E (((_,"bnode"),_), [D s])) ::_ ->
          Some (Rdf_term.Blank_ (Rdf_term.blank_id_of_string s))
      | (E (((_,"literal"),atts), [D s])) ::_ ->
          let typ =
            match get_att ("","datatype") atts with
              None -> None
            | Some s -> Some (Iri.of_string s)
          in
          let lang = get_att (Xmlm.ns_xml, "lang") atts in
          Some (Rdf_term.term_of_literal_string ?typ ?lang s)
      | _ :: q -> iter q
      in
      iter

    let binding_of_xml = function
    | E (((_,"binding"), atts), subs) ->
        begin
          match get_binding_name atts with
            None -> raise (Invalid_response ("missing binding name", ""))
          | Some name ->
              match term_of_xmls subs with
                None -> raise (Invalid_response ("missing binding term", ""))
              | Some term -> Some (name, term)
        end
    | _ -> None

    let solution_of_xmls xmls =
      let mu = Rdf_sparql_ms.mu_0 in
      let rec iter acc = function
        [] -> Rdf_sparql.solution_of_mu acc
      | xml :: q ->
          match binding_of_xml xml with
            None -> iter acc q
          | Some (name,term) ->
              let acc = Rdf_sparql_ms.mu_add name term acc in
              iter acc q
      in
      iter mu xmls

    let results_of_xmls =
      let rec iter acc = function
        [] -> List.rev acc
      | xml :: q ->
          match xml with
            E (((_,"result"),_), xmls) ->
              iter ((solution_of_xmls xmls) :: acc) q
          | _ -> iter acc q
      in
      iter []

    let result_of_xml xml =
      match first_child xml "boolean" with
        Some (D _) -> assert false
      | Some (E (_, [ D s ])) ->
          Rdf_sparql.Bool (String.lowercase s = "true")
      | Some (E _) -> raise (Invalid_response ("bad boolean content", "<...>...</...>"))
      | None ->
          match first_child xml "results" with
            None -> raise (Invalid_response ("no <results> node", ""))
          | Some (D _) -> assert false
           (* try to get a <boolean> node first, if not, we look for solutions *)
          | Some (E (_, xmls) as x) ->
              match first_child x "boolean" with
              | Some (D _) -> assert false
              | Some (E (_, [ D s ])) ->
                  Rdf_sparql.Bool (String.lowercase s = "true")
              | Some (E _) ->
                  raise (Invalid_response ("bad boolean content", "<...>...</...>"))
              | None ->
                  let solutions = results_of_xmls xmls in
                  Rdf_sparql.Solutions solutions
  end;;

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
  end;;

module type S =
  sig
    type result
    val get : ?graph: Rdf_graph.graph -> base:Iri.t -> ?accept: string ->
      Uri.t -> Rdf_sparql_protocol.in_message -> result
    val post : ?graph: Rdf_graph.graph -> base:Iri.t -> ?accept: string ->
      Uri.t -> ?query_var: string ->
      Rdf_sparql_protocol.in_message -> result
  end

module Make (P : P) =
  struct
    type result = Rdf_sparql_protocol.out_message P.t

    let read_rdf_xml ?graph ~base xml =
      begin
        let g =
          match graph with
            Some g -> g
          | None -> Rdf_graph.open_graph base
        in
        try
          Rdf_xml.from_xml g ~base xml;
          Rdf_sparql_protocol.Result (Rdf_sparql.Graph g)
        with
          Rdf_xml.Invalid_rdf s ->
            raise (Invalid_response (s, ""))
      end

    let result_of_string ?graph ~base ~content_type body =
      (*print_endline ("Content-Type received = "^content_type);*)
      (* get rid of charset=... eventually *)
      let content_type =
        match Rdf_misc.split_string content_type [';'] with
         [] -> content_type
       | h :: _ -> h
      in
      match content_type with
      | "application/xml"
      | "text/xml"
      | "application/sparql-results+xml" ->
          begin
            let xml =
              try Rdf_xml.xml_of_string body
              with Failure msg -> raise (Invalid_response (msg, body))
            in
            try Rdf_sparql_protocol.Result (Xml.result_of_xml xml)
            with Invalid_response (msg,_) ->
                (* it may not be a solution or boolean, but rather an rdf
                   graph, let's try to load it*)
                read_rdf_xml ?graph ~base xml
          end
      | "application/rdf+xml" ->
          begin
            let xml =
              try Rdf_xml.xml_of_string body
              with Failure msg -> raise (Invalid_response (msg, body))
            in
            read_rdf_xml ?graph ~base xml
          end

      | "application/x-turtle"
      | "text/turtle" ->
          begin
            let g =
              match graph with
                Some g -> g
              | None -> Rdf_graph.open_graph base
            in
            try
              Rdf_ttl.from_string g ~base body;
              Rdf_sparql_protocol.Result (Rdf_sparql.Graph g)
           with
              Rdf_ttl.Error e ->
                raise (Invalid_response (Rdf_ttl.string_of_error e, body))
          end
      | "application/sparql-results+json"
      | "application/json" ->
          begin
            try
              let json = Yojson.Basic.from_string body in
              let res = Rdf_json.sparql_result_of_json json in
              Rdf_sparql_protocol.Result res
            with
              Rdf_json.Unexpected_json (s,_) ->
                raise (Invalid_response (s, body))
          end
      | "text/plain" ->
          begin
            match String.lowercase body with
            | "true" -> Rdf_sparql_protocol.Result (Rdf_sparql.Bool true)
            | "false" -> Rdf_sparql_protocol.Result (Rdf_sparql.Bool false)
            | _-> Rdf_sparql_protocol.Ok
          end
      | s -> raise (Unsupported_content_type s)

    let default_accept =
      "application/xml, text/xml, application/sparql-results+xml, application/rdf+xml,"^
      "application/x-turtle, text/turtle, "^
      "application/sparql-results+json, application/json, " ^
      "text/plain"

    let make_query_string ?(query_var="query") msg =
      let enc_v = Uri.pct_encode ~component: `Query_value in
      let regexp = Str.regexp "[\n]+" in
      let spql_query = Str.global_replace regexp " "
        (query_var^"="^(enc_v msg.in_query))
      in
      let ds = msg.in_dataset in
      let l =
        (match ds.inds_default with
           None -> []
         | Some iri -> ["default-graph-uri="^(Iri.to_string iri)]) @
          (List.map (fun iri -> "named-graph-uri="^(Iri.to_string iri)) ds.inds_named)
      in
      match l with
        [] -> spql_query
      | _ -> spql_query^"&"^(String.concat "&" l)

    let get ?graph ~base ?(accept=default_accept) uri msg =
      let query = make_query_string msg in
      let query = Uri.query_of_encoded query in
      let uri = Uri.with_query uri query in
      P.get uri ~accept (result_of_string ?graph ~base)

    let post ?graph ~base ?(accept=default_accept) uri ?query_var msg =
      let query = make_query_string ?query_var msg in
      P.post uri ~accept
        ~content_type: "application/x-www-form-urlencoded"
        ~content: query
        (result_of_string ?graph ~base)

  end

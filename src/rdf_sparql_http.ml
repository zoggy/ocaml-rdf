(** HTTP bindings for Sparql protocol.

  [http://www.w3.org/TR/rdf-sparql-protocol/#query-bindings-http]

  Using [http://www.w3.org/2005/sparql-results#] as reference for
  representation of Sparql results.
*)

open Rdf_sparql_protocol
open Rdf_xml

exception Unsupported_content_type of string
exception Invalid_response of string * string (** error * body *)

(* sparql-results namespace *)
let spr_ns = "http://www.w3.org/2005/sparql-results#"

let xml_first_child xml tag =
  match xml with
    D _ -> None
  | E ((_,_),subs) ->
      try Some (List.find (function E (((_,t),_),_) -> t = tag | _ -> false) subs)
      with Not_found -> None
;;

module Xml =
  struct
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
            | Some s -> Some (Rdf_iri.iri s)
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
  end;;

module type P =
  sig
    type 'a t
    val get : Rdf_uri.uri -> ?accept: string ->
      (content_type:string -> string -> Rdf_sparql_protocol.out_message) ->
        Rdf_sparql_protocol.out_message t
(*    val post : Rdf_uri.uri ->
      ?accept: string -> content_type: string -> content: string ->
        (content_type: string -> string ->  Rdf_sparql_protocol.out_message) ->
        Rdf_sparql_protocol.out_message t
*)
  end;;

module type S =
  sig
    type result
    val get : ?graph: Rdf_graph.graph -> base:Rdf_iri.iri ->
      Rdf_uri.uri -> Rdf_sparql_protocol.in_message -> result
    val post : ?graph: Rdf_graph.graph -> base:Rdf_iri.iri ->
      Rdf_uri.uri -> Rdf_sparql_protocol.in_message -> result
  end

module Make (P : P) =
  (*: S with type result = Rdf_sparql_protocol.out_message P.t *)
  struct
    type result = Rdf_sparql_protocol.out_message P.t

    let solution_of_xmls xmls =
      let mu = Rdf_sparql_ms.mu_0 in
      let rec iter acc = function
        [] -> Rdf_sparql.solution_of_mu acc
      | xml :: q ->
          match Xml.binding_of_xml xml with
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
      match xml_first_child xml "boolean" with
        Some (D s) ->
          Rdf_sparql.Bool (String.lowercase s = "true")
      | Some (E _) -> raise (Invalid_response ("bad boolean content", "<...>...</...>"))
      | None ->
          match xml_first_child xml "results" with
            None -> raise (Invalid_response ("no <results> node", ""))
          | Some (D _) -> assert false
          | Some (E (_, xmls)) ->
              let solutions = results_of_xmls xmls in
              Rdf_sparql.Solutions solutions

    let result_of_string ?graph ~base ~content_type body =
      match content_type with
       | "application/sparql-results+xml" ->
          begin
            let xml =
              try Rdf_xml.xml_of_string body
              with Failure msg -> raise (Invalid_response (msg, body))
            in
            Rdf_sparql_protocol.Result (result_of_xml xml)
          end
      | "application/rdf+xml" ->
          begin
            let g =
              match graph with
                Some g -> g
              | None -> Rdf_graph.open_graph base
            in
            try
              Rdf_xml.from_string g ~base body;
              Rdf_sparql_protocol.Result (Rdf_sparql.Graph g)
           with
              Rdf_xml.Invalid_rdf s ->
                raise (Invalid_response (s, body))
          end
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
      | s -> raise (Unsupported_content_type s)

    let get ?graph ~base uri msg =
      let url = Rdf_uri.neturl uri in
      let enc = Netencoding.Url.encode in
      let query =
        let spql_query = "query="^(enc msg.in_query) in
        let ds = msg.in_dataset in
        let l =
          (match ds.inds_default with
             None -> []
           | Some iri -> ["default-graph-uri="^(Rdf_iri.string iri)]) @
            (List.map (fun iri -> "named-graph-uri="^(Rdf_iri.string iri)) ds.inds_named)
        in
        match l with
          [] -> spql_query
        | _ -> spql_query^"&"^(String.concat "&" l)
      in
      let url = Neturl.modify_url ~query ~encoded: true url in
      let uri = Rdf_uri.of_neturl url in
      P.get uri (result_of_string ?graph ~base)

    let post ?graph ~base uri msg = assert false



  end
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

module Yojson = Yojson.Basic

exception Unexpected_json of string * Yojson.json

let mk_term v = function
  | "uri"                       -> Rdf_term.term_of_iri_string v
  | "literal"                   -> Rdf_term.(Literal (mk_literal v))
  | _ when String.length v != 0 -> Rdf_term.(Blank_ (blank_id_of_string v))
  | _                           -> Rdf_term.Blank

let term_of_json json =
  try
    let e_type = Yojson.Util.(to_string (member "type" json)) in
    let value = Yojson.Util.(to_string (member "value" json)) in
    mk_term value e_type
  with
    Yojson.Util.Type_error (s,_) -> raise (Unexpected_json (s, json))

(** {2 Serializing SPARQL Query Results in JSON}

  [http://www.w3.org/TR/rdf-sparql-json-res/]
*)

let couple_of_json mu (name, j_term) =
  Rdf_sparql_ms.mu_add name (term_of_json j_term) mu

let solution_of_json json =
  Rdf_sparql.solution_of_mu
    (List.fold_left couple_of_json Rdf_sparql_ms.mu_0
       (Yojson.Util.to_assoc json))

let solutions_of_json json =
  List.map solution_of_json (Yojson.Util.to_list json)

(*
let string_of_json string_list json =
  (Yojson.Util.to_string json)::string_list

let head_of_json json =
  List.fold_left string_of_json [] (Yojson.Util.to_list json)
*)

let sparql_result_of_json json =
  match Yojson.Util.member "boolean" json with
    `Bool b -> Rdf_sparql.Bool b
  | `Null ->
      begin
        try
          let results_assoc = Yojson.Util.(member "results" json) in
          let bindings = Yojson.Util.(member "bindings" results_assoc) in
          Rdf_sparql.Solutions (solutions_of_json bindings)
        with
          Yojson.Util.Type_error (s, _) -> raise (Unexpected_json (s, json))
      end
  | _ -> raise (Unexpected_json ("query result", json))
;;


  
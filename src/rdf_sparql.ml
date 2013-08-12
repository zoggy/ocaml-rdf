(*********************************************************************************)
(*                OCaml-RDF                                                      *)
(*                                                                               *)
(*    Copyright (C) 2012-2013 Institut National de Recherche en Informatique     *)
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

open Rdf_sparql_types;;
open Rdf_sparql_eval;;

type error =
| Parse_error of Rdf_loc.loc * string
| Value_error of Rdf_dt.error
| Eval_error of Rdf_sparql_eval.error
| Algebra_error of Rdf_sparql_algebra.error
| Not_select
| Not_ask
| Not_construct
| Not_describe

exception Error of error
let error e = raise (Error e)

let rec string_of_error = function
  Parse_error (loc, s) ->
    Printf.sprintf "%s%s" (Rdf_loc.string_of_loc loc) s

| Value_error (Rdf_dt.Exception (Rdf_dt.Error e)) ->
    string_of_error (Value_error e)

| Value_error (Rdf_dt.Exception (Rdf_sparql_eval.Error e))
| Eval_error e -> Rdf_sparql_eval.string_of_error e

| Value_error e -> Rdf_dt.string_of_error e
| Algebra_error e -> Rdf_sparql_algebra.string_of_error e

| Not_select -> "Query is not a SELECT"
| Not_ask -> "Query is not a ASK"
| Not_construct -> "Query is not a CONSTRUCT"
| Not_describe -> "Query is not a DESCRIBE"
;;


let parse_from_lexbuf source_info ?fname lexbuf =
  let parse = Rdf_ulex.menhir_with_ulex Rdf_sparql_parser.query Rdf_sparql_lex.main ?fname in
  let q =
    try parse lexbuf
    with
    (*| MenhirLib.TableInterpreter.Accept _ *)
    | Rdf_sparql_parser.Error ->
        let (start, stop) = Ulexing.loc lexbuf in
        let loc = source_info start stop in
        let lexeme = Ulexing.utf8_lexeme lexbuf in
        let msg = Printf.sprintf "Parse error on lexeme %S" lexeme in
        raise (Error (Parse_error (loc, msg)))
    | Failure msg ->
        let (start, stop) = Ulexing.loc lexbuf in
        let loc = source_info start stop in
        raise (Error (Parse_error (loc, msg)))
  in
  q

let parse_from_string s =
  let lexbuf = Ulexing.from_utf8_string s in
  parse_from_lexbuf (Rdf_loc.source_info_string s) lexbuf
;;

let parse_from_file file =
  let ic = open_in file in
  let lexbuf = Ulexing.from_utf8_channel ic in
  try parse_from_lexbuf (Rdf_loc.source_info_file file) ~fname: file lexbuf
  with e ->
      close_in ic;
      raise e
;;

let string_of_query q =
  let b = Buffer.create 256 in
  Rdf_sparql_print.print_query b q ;
  Buffer.contents b
;;


let dbg = Rdf_misc.create_log_fun
  ~prefix: "Rdf_sparql_eval"
    "RDF_SPARQL_QUERY_DEBUG_LEVEL"
;;

type solution = Rdf_sparql_ms.mu

type query_result =
  Bool of bool
| Solutions of Rdf_sparql_ms.mu list
| Graph of Rdf_graph.graph

let construct_graph graph solutions = ();;

let execute ?graph ~base dataset query =
  let (base, ds, query) = Rdf_sparql_expand.expand_query base query in
  let q =
    match query.q_kind with
      Select s ->
        { Rdf_sparql_algebra.query_proj = Some s.select_select ;
          query_where = s.select_where ;
          query_modifier = s.select_modifier ;
          query_values = None ;
        }
    | Ask a ->
        { Rdf_sparql_algebra.query_proj = None ;
          query_where = a.ask_where ;
          query_modifier = a.ask_modifier ;
          query_values = None ;
        }
    | Construct c ->
        let w =
          match c.constr_where with
            Constr_ggp ggp -> ggp
          | Constr_template _ -> assert false (* FIXME: implement *)
        in
        { Rdf_sparql_algebra.query_proj = None ;
          query_where = w ;
          query_modifier = c.constr_modifier ;
          query_values = None ;
        }
    | Describe d ->
        let w =
          match d.desc_where with
          | None -> GGPSub { ggp_sub_loc = Rdf_loc.dummy_loc ; ggp_sub_elts = [] }
          | Some w -> w
        in
        { Rdf_sparql_algebra.query_proj = None ; (* FIXME: handle desc_sel *)
          query_where = w ;
          query_modifier = d.desc_modifier ;
          query_values = None ;
        }
  in
  let algebra = Rdf_sparql_algebra.translate_query_level q in
  dbg ~level: 2 (fun () -> Rdf_sparql_algebra.string_of_algebra algebra);
  dbg ~level: 2 (fun () -> Rdf_ttl.to_string dataset.Rdf_ds.default);
  let ctx = Rdf_sparql_eval.context ~base
    ?from: ds.Rdf_sparql_expand.from
        ~from_named: ds.Rdf_sparql_expand.from_named dataset
  in
  let solutions = Rdf_sparql_eval.eval_list ctx algebra in
  match query.q_kind with
    Select _ -> Solutions solutions
  | Ask _ -> Bool (solutions <> [])
  | Construct _ ->
      let g =
        match graph with
          Some g -> g
        | None -> Rdf_graph.open_graph base
      in
      construct_graph g solutions ;
      Graph g
  | Describe _ -> assert false
;;

let execute ?graph ~base dataset query =
  try execute ?graph ~base dataset query
  with
    Rdf_dt.Error e -> error (Value_error e)
  | Rdf_sparql_eval.Error e -> error (Eval_error e)
  | Rdf_sparql_algebra.Error e -> error (Algebra_error e)
;;

let select ~base dataset query =
  match query.q_kind with
    Select _ ->
      (match execute ~base dataset query with
         Solutions l -> l
       | _ -> assert false
      )
  | _ -> error Not_select
;;

let construct ?graph ~base dataset query =
  match query.q_kind with
    Construct _ ->
      (match execute ?graph ~base dataset query with
         Graph g -> g
       | _ -> assert false
      )
  | _ -> error Not_construct
;;

let ask ~base dataset query =
  match query.q_kind with
    Ask _ ->
      (match execute ~base dataset query with
         Bool b -> b
       | _ -> assert false
      )
  | _ -> error Not_ask
;;

let describe ~base dataset query =
  match query.q_kind with
    Describe _ ->
      (match execute ~base dataset query with
        _ -> assert false
      )
  | _ -> error Not_describe
;;

  
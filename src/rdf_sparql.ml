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

open Rdf_sparql_types;;
open Rdf_sparql_eval;;

let dbg = Rdf_misc.create_log_fun
  ~prefix: "Rdf_sparql"
    "RDF_SPARQL_DEBUG_LEVEL"
;;

type error =
| Parse_error of Rdf_loc.loc * string
| Value_error of Rdf_dt.error
| Eval_error of Rdf_sparql_eval.error
| Algebra_error of Rdf_sparql_algebra.error
| Not_select
| Not_ask
| Not_construct
| Not_describe
| Not_get
| Not_update
| Not_implemented of string

exception Error of error
let error e = raise (Error e)

let rec string_of_error = function
  Parse_error (loc, s) ->
    (Rdf_loc.string_of_loc loc) ^ s

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
| Not_get -> "Query is not a (SELECT|ASK|CONSTRUCT|DESCRIBE)"
| Not_update -> "Query is not an update query"
| Not_implemented str -> Printf.sprintf "%s: Not implemented" str
;;

let () = Printexc.register_printer
  (function Error e -> Some (string_of_error e) | _ -> None)

let query_from_lexbuf ?fname lexbuf =
  let parse = Rdf_sedlex.menhir_with_ulex Rdf_sparql_parser.query Rdf_sparql_lex.main ?fname in
  let q =
    try parse lexbuf
    with Rdf_sedlex.Parse_error (e, pos)->
        let msg =
          match e with
            Rdf_sparql_parser.Error ->
              let lexeme = Sedlexing.Utf8.lexeme lexbuf in
              Printf.sprintf "Parse error on lexeme %S" lexeme
          | Failure msg ->
              msg
          | Iri.Error e -> Iri.string_of_error e
          | e -> Printexc.to_string e
        in
        let loc = { Rdf_loc.loc_start = pos ; loc_end = pos } in
        error (Parse_error (loc,msg))
  in
  q

type query = Rdf_sparql_types.query

let query_from_string s =
  let lexbuf = Sedlexing.Utf8.from_string s in
  query_from_lexbuf lexbuf
;;

let query_from_file file =
  let ic = open_in file in
  let lexbuf = Sedlexing.Utf8.from_channel ic in
  try query_from_lexbuf ~fname: file lexbuf
  with e ->
      close_in ic;
      raise e
;;

let string_of_query q =
  let b = Buffer.create 256 in
  Rdf_sparql_print.print_query b q ;
  Buffer.contents b
;;

type solution = Rdf_sparql_ms.mu

let solution_of_mu x = x;;

let get_term sol v = Rdf_sparql_ms.mu_find_varname v sol;;
let get_string_literal sol v =
  Rdf_dt.string_literal (Rdf_dt.of_term (get_term sol v))
;;

let get_string sol v =
  match Rdf_dt.string (Rdf_dt.of_term (get_term sol v)) with
    Rdf_dt.String s -> s
  | Rdf_dt.Err e -> raise (Rdf_dt.Error e)
  | _ -> assert false
;;

let get_iri sol base v =
  match Rdf_dt.iri base (Rdf_dt.of_term (get_term sol v)) with
    Rdf_dt.Iri iri -> iri
  | Rdf_dt.Err e -> raise (Rdf_dt.Error e)
  | _ -> assert false
;;

let get_int sol v =
  match Rdf_dt.int (Rdf_dt.of_term (get_term sol v)) with
    Rdf_dt.Int (n, _) -> n
  | Rdf_dt.Err e -> raise (Rdf_dt.Error e)
  | _ -> assert false
;;

let get_float sol v =
  match Rdf_dt.float (Rdf_dt.of_term (get_term sol v)) with
    Rdf_dt.Float d -> d
  | Rdf_dt.Err e -> raise (Rdf_dt.Error e)
  | _ -> assert false
;;

let get_bool sol v =
  match Rdf_dt.bool (Rdf_dt.of_term (get_term sol v)) with
    Rdf_dt.Bool b-> b
  | Rdf_dt.Err e -> raise (Rdf_dt.Error e)
  | _ -> assert false
;;

let get_datetime sol v =
  match Rdf_dt.datetime (Rdf_dt.of_term (get_term sol v)) with
    Rdf_dt.Datetime t -> t
  | Rdf_dt.Err e -> raise (Rdf_dt.Error e)
  | _ -> assert false
;;

let get_ltrl sol v =
  match Rdf_dt.ltrl (Rdf_dt.of_term (get_term sol v)) with
    Rdf_dt.Ltrl (s,lang) -> (s, lang)
  | Rdf_dt.Err e  -> raise (Rdf_dt.Error e)
  | _ -> assert false
;;


let is_bound sol v = try ignore(get_term sol v); true with Not_found -> false;;
let solution_fold = Rdf_sparql_ms.mu_fold ;;
let solution_iter = Rdf_sparql_ms.mu_iter ;;

type query_result =
  Bool of bool
| Solutions of Rdf_sparql_ms.mu list
| Graph of Rdf_graph.graph
;;

let construct_template c =
  match c.constr_template with
    Some t -> t
  | None ->
      match c.constr_where with
        Constr_ggp _ -> assert false
      | Constr_template t -> t
;;

let construct_graph graph template solutions =
  dbg ~level: 1
    (fun () -> "construct graph with "^(string_of_int (List.length solutions))^" solution(s)");
  List.iter (Rdf_update.add_solution_to_graph graph template) solutions
;;

let construct_project_vars =
  let f_var f acc v = Rdf_sparql_types.VarSet.add v acc in
  let visitor = { Rdf_sparql_vis.default with Rdf_sparql_vis.var = f_var } in
  let map_to_selvar v =
    { sel_var_loc = v.var_loc ; sel_var_expr = None ; sel_var = v }
  in
  fun template ->
    let vars = List.fold_left (visitor.Rdf_sparql_vis.triples_same_subject visitor)
      Rdf_sparql_types.VarSet.empty template
    in
    List.map  map_to_selvar (Rdf_sparql_types.VarSet.elements vars)
;;

let execute_get ?graph ~base dataset query =
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
        let template = construct_template c in
        let w =
          match c.constr_where with
            Constr_ggp ggp -> ggp
          | Constr_template triples ->
              let loc = Rdf_loc.dummy_loc in
              GGPSub
                { ggp_sub_loc = loc ;
                  ggp_sub_elts =
                    [
                      Triples { triples_loc = loc ; triples = triples }
                    ]
                }
        in
        (* project solutions according to variables used in template *)
        let proj =
          { sel_flag = None ;
            sel_vars = SelectVars (construct_project_vars template) ;
          }
        in
        { Rdf_sparql_algebra.query_proj = Some proj ;
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
    | Update _ -> assert false
  in
  let algebra = Rdf_sparql_algebra.translate_query_level q in
  dbg ~level: 2 (fun () -> Rdf_sparql_algebra.string_of_algebra algebra);
  dbg ~level: 4 (fun () -> Rdf_ttl.to_string dataset.Rdf_ds.default);
  let ctx = Rdf_sparql_eval.context ~base
    ~from: ds.Rdf_sparql_expand.from
        ~from_named: ds.Rdf_sparql_expand.from_named dataset
  in
  let solutions = Rdf_sparql_eval.eval_list ctx algebra in
  match query.q_kind with
    Select _ -> Solutions solutions
  | Ask _ -> Bool (solutions <> [])
  | Construct c ->
      let template = construct_template c in
      let g =
        match graph with
          Some g -> g
        | None -> Rdf_graph.open_graph base
      in
      construct_graph g template solutions ;
      Graph g
  | Describe _ ->
      let g =
        match graph with
          Some g -> g
        | None -> Rdf_graph.open_graph base
      in
      (* FIXME: fill graph *)
      Graph g
  | Update _ -> assert false
;;

let execute_update ~graph = function
| Update_load -> error (Not_implemented "LOAD")
| Update_clear -> error (Not_implemented "LOAD")
| Update_drop -> error (Not_implemented "LOAD")
| Update_add -> error (Not_implemented "LOAD")
| Update_move -> error (Not_implemented "LOAD")
| Update_copy -> error (Not_implemented "COPY")
| Update_create  -> error (Not_implemented "CREATE")
| Update_insert_data qd -> Rdf_update.insert_data ~graph qd
| Update_delete_data qd -> Rdf_update.delete_data ~graph qd
| Update_delete_where qp -> Rdf_update.delete_where ~graph qp
| Update_modify m -> Rdf_update.modify ~graph m

let execute_update ~graph q =
  match q.q_kind with
    Update actions ->
      begin
       try Bool (List.for_all (execute_update ~graph) actions)
       with
          Rdf_dt.Error e -> error (Value_error e)
        | Rdf_sparql_eval.Error e -> error (Eval_error e)
        | Rdf_sparql_algebra.Error e -> error (Algebra_error e)
      end
  | _ -> error Not_update

let execute ?graph ~base dataset query =
  match query.q_kind with
    Update _ -> error Not_get
  | _ -> execute_get ?graph ~base dataset query

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

let describe ?graph ~base dataset query =
  match query.q_kind with
    Describe _ ->
      (match execute ?graph ~base dataset query with
        _ -> assert false
      )
  | _ -> error Not_describe
;;

type iri_fun = Rdf_dt.value list -> Rdf_dt.value

let iri_funs () = !Rdf_sparql_eval.iri_funs;;
let add_iri_fun = Rdf_sparql_eval.add_iri_fun;;


  
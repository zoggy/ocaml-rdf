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
;;


let query_from_lexbuf source_info ?fname lexbuf =
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

type query = Rdf_sparql_types.query

let query_from_string s =
  let lexbuf = Ulexing.from_utf8_string s in
  query_from_lexbuf (Rdf_loc.source_info_string s) lexbuf
;;

let query_from_file file =
  let ic = open_in file in
  let lexbuf = Ulexing.from_utf8_channel ic in
  try query_from_lexbuf (Rdf_loc.source_info_file file) ~fname: file lexbuf
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
    Rdf_dt.Iri uri -> uri
  | Rdf_dt.Err e -> raise (Rdf_dt.Error e)
  | _ -> assert false
;;

let get_int sol v =
  match Rdf_dt.int (Rdf_dt.of_term (get_term sol v)) with
    Rdf_dt.Int n -> n
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

module SMap = Rdf_sparql_types.SMap;;

let var_or_term_apply_sol sol bnode_map = function
  Rdf_sparql_types.Var v ->
    (
     try
       let node = Rdf_sparql_ms.mu_find_var v sol in
       match node with
         Rdf_term.Blank_ label ->
           begin
             let label = Rdf_term.string_of_blank_id label in
             let (label, bnode_map) =
               try (SMap.find label bnode_map, bnode_map)
               with Not_found ->
                   let new_label = Rdf_sparql_ms.gen_blank_id () in
                   let map = SMap.add label new_label bnode_map in
                   (new_label, map)
             in
             (Rdf_term.Blank_ (Rdf_term.blank_id_of_string label), bnode_map)
        end
       | Rdf_term.Blank -> assert false
       | node -> (node, bnode_map)
     with Not_found ->
       failwith ("Unbound variable "^v.var_name)
    )
| Rdf_sparql_types.GraphTerm t ->
    match t with
    | GraphTermIri (PrefixedName _) -> assert false
    | GraphTermIri (Iriref ir) -> (Rdf_term.Uri (ir.ir_iri), bnode_map)
    | GraphTermLit lit
    | GraphTermNumeric lit
    | GraphTermBoolean lit -> (Rdf_term.Literal lit.rdf_lit, bnode_map)
    | GraphTermBlank { bnode_label = None }
    | GraphTermNil ->
       let label = Rdf_sparql_ms.gen_blank_id () in
       (Rdf_term.Blank_ (Rdf_term.blank_id_of_string label), bnode_map)
    | GraphTermBlank { bnode_label = Some label } ->
        begin
          let (label, bnode_map) =
            try (SMap.find label bnode_map, bnode_map)
            with Not_found ->
                let new_label = Rdf_sparql_ms.gen_blank_id () in
                let map = SMap.add label new_label bnode_map in
                (new_label, map)
          in
          (Rdf_term.Blank_ (Rdf_term.blank_id_of_string label), bnode_map)
        end
    | GraphTermNode _ -> assert false
;;

let add_solution_to_graph graph template =
  let triples =
    List.fold_left
      Rdf_sparql_algebra.translate_triples_same_subject_path [] template
  in
  dbg ~level: 2
    (fun () -> "construct "^(string_of_int (List.length triples))^" triple(s) per solution");
  let build_triple sol (triples, bnode_map) (sub, path, obj) =
    try
      let pred =
        match path with
          Rdf_sparql_algebra.Var v -> Rdf_sparql_types.Var v
        | Rdf_sparql_algebra.Iri iriref ->
            Rdf_sparql_types.GraphTerm
              (Rdf_sparql_types.GraphTermIri (Rdf_sparql_types.Iriref iriref))
        | _ -> failwith "Invalid predicate spec in template"
      in
      let (sub, bnode_map) =
        let (node, bnode_map) = var_or_term_apply_sol sol bnode_map sub in
        match node with
          Rdf_term.Literal _ -> failwith "Invalid subject (literal)"
        | _ -> (node, bnode_map)
      in
      let (pred, bnode_map) =
        let (node, bnode_map) = var_or_term_apply_sol sol bnode_map pred in
        match node with
        | Rdf_term.Uri uri -> (uri, bnode_map)
        | Rdf_term.Literal _ -> failwith "Invalid predicate (literal)"
        | Rdf_term.Blank | Rdf_term.Blank_ _ -> failwith "Invalid predicate (blank)"
      in
      let (obj, bnode_map) = var_or_term_apply_sol sol bnode_map obj in
      ((sub, pred, obj) :: triples, bnode_map)
    with
      e ->
        dbg ~level: 2 (fun _ -> Printexc.to_string e);
        (triples, bnode_map)
  in
  let insert (sub,pred,obj) = graph.Rdf_graph.add_triple ~sub ~pred ~obj in
  let f sol =
    (*Rdf_sparql_ms.SMap.iter
      (fun name term -> print_string (name^"->"^(Rdf_term.string_of_node term)^" ; "))
      sol.Rdf_sparql_ms.mu_bindings;
    print_newline();
    *)
    let (triples,_) =
      List.fold_left (build_triple sol) ([], SMap.empty) triples
    in
    List.iter insert triples
  in
  f
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
  List.iter (add_solution_to_graph graph template) solutions
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
  in
  let algebra = Rdf_sparql_algebra.translate_query_level q in
  dbg ~level: 2 (fun () -> Rdf_sparql_algebra.string_of_algebra algebra);
  dbg ~level: 4 (fun () -> Rdf_ttl.to_string dataset.Rdf_ds.default);
  let ctx = Rdf_sparql_eval.context ~base
    ?from: ds.Rdf_sparql_expand.from
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
      Graph g
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

let describe ?graph ~base dataset query =
  match query.q_kind with
    Describe _ ->
      (match execute ?graph ~base dataset query with
        _ -> assert false
      )
  | _ -> error Not_describe
;;

type uri_fun = Rdf_dt.value list -> Rdf_dt.value

let uri_funs () = !Rdf_sparql_eval.uri_funs;;
let add_uri_fun = Rdf_sparql_eval.add_uri_fun;;


  
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

(** MySQL storage 1. *)

open Rdf_term;;
open Rdf_my;;

let dbg = Rdf_my.dbg;;

let table_options engine = " ENGINE="^engine^" DELAY_KEY_WRITE=1 MAX_ROWS=100000000 DEFAULT CHARSET=UTF8";;
let creation_queries =
  [
    "CREATE TABLE IF NOT EXISTS graphs (id integer AUTO_INCREMENT PRIMARY KEY NOT NULL, name text NOT NULL)" ;
    "CREATE TABLE IF NOT EXISTS bnodes (id bigint PRIMARY KEY NOT NULL, value text NOT NULL) AVG_ROW_LENGTH=33" ;
    "CREATE TABLE IF NOT EXISTS resources (id bigint PRIMARY KEY NOT NULL, value text NOT NULL) AVG_ROW_LENGTH=80";
    "CREATE TABLE IF NOT EXISTS literals (id bigint PRIMARY KEY NOT NULL, value longtext NOT NULL,
                                          language text, datatype text)  AVG_ROW_LENGTH=50" ;
  ]
;;

let prepared_term_of_hash = "term_of_hash";;

let hash_of_term dbd ?(add=false) term =
  let hash = Rdf_term.term_hash term in
  if add then
    begin
      let test_query =
        "SELECT COUNT(*) FROM " ^
          (match term with
             Uri _ -> "resources"
           | Literal _ -> "literals"
           | Blank_ _ | Blank -> "bnodes"
          ) ^
          " WHERE id=" ^
          (Int64.to_string hash)
      in
      match Mysql.fetch (exec_query dbd test_query) with
        Some [| Some s |] when int_of_string s = 0 ->
          let pre_query =
            match term with
              Uri uri ->
                "resources (id, value) values ("^
                  (Int64.to_string hash) ^
                  ", " ^ (quote_str (Rdf_uri.string uri)) ^ ")"
            | Literal lit ->
                "literals (id, value, language, datatype) values (" ^
                  (Int64.to_string hash) ^ ", " ^
                  (quote_str lit.lit_value) ^ ", " ^
                  (quote_str (Rdf_misc.string_of_opt lit.lit_language)) ^ ", " ^
                  (quote_str (Rdf_misc.string_of_opt (Rdf_misc.map_opt Rdf_uri.string lit.lit_type))) ^ ")"
            | Blank_ id ->
                "bnodes (id, value) values (" ^
                (Int64.to_string hash) ^ ", " ^
                  (quote_str (Rdf_term.string_of_blank_id id)) ^ ")"
            | Blank -> assert false
          in
          let query = "INSERT INTO " ^ pre_query (* ON DUPLICATE KEY UPDATE value=value*) in
          ignore(exec_query dbd query)
      | _ -> ()
    end;
  hash
;;

let init_db db engine =
  let table_options = table_options engine in
  let queries = List.map (fun q -> q^table_options) creation_queries in
  Rdf_my.init_db db queries
;;

let to_uri = function
  Rdf_term.Uri uri -> uri
| t -> failwith ("Not a URI:"^(Rdf_term.string_of_term t))
;;

let init_graph dbd engine name =
  let table = Rdf_my.graph_table_of_graph_name dbd name in
  if not (Rdf_my.table_exists dbd table) then
    begin
      let query =
        "CREATE TABLE IF NOT EXISTS "^table^" (\
         subject bigint NOT NULL, predicate bigint NOT NULL, \
         object bigint NOT NULL,\
         KEY SubjectPredicate (subject,predicate),\
         KEY PredicateObject (predicate,object),\
         KEY SubjectObject (subject,object)\
        ) "^(table_options engine)^" AVG_ROW_LENGTH=59"
      in
      ignore(Rdf_my.exec_query dbd query);
(*
      let query = Printf.sprintf
        "ALTER TABLE %s ADD UNIQUE INDEX (subject, predicate, object)" table
      in
      ignore(exec_query dbd query)
*)
    end;
  let more = [
      prepared_term_of_hash,
      "SELECT NULL, value, NULL, NULL, NULL FROM resources where id=? LIMIT 1 UNION ALL \
      SELECT NULL, NULL, value, language, datatype FROM literals where id=? LIMIT 1 UNION ALL \
      SELECT value, NULL, NULL, NULL, NULL FROM bnodes where id=? LIMIT 1" ;
    ]
  in
  Rdf_my.prepare_queries dbd ~more table;
  table
;;

let term_of_hash dbd hash =
  let s_hash = Int64.to_string hash in
  let res = Rdf_my.exec_prepared dbd prepared_term_of_hash [ s_hash ; s_hash ; s_hash ] in
  let size = Mysql.size res in
  match Int64.compare size Int64.one with
    n when n > 0 ->
      let msg = "No term with hash \"" ^(Int64.to_string hash)^ "\"" in
      raise (Error msg)
  | 0 ->
      begin
        match Mysql.fetch res with
          None -> assert false (* already tested: there is at least one row *)
        | Some t ->
            match t with
              [| Some name ; None ; None ; None ; None |] ->
                Blank_ (Rdf_term.blank_id_of_string name)
            | [| None ; Some uri ; None ; None ; None |] ->
                Rdf_term.term_of_uri_string uri
            | [| None ; None ; Some value ; lang ; typ |] ->
                let typ = Rdf_misc.map_opt
                  (Rdf_uri.uri ~check: false)
                  (Rdf_misc.opt_of_string (Rdf_misc.string_of_opt typ))
                in
                Rdf_term.term_of_literal_string
                ?lang: (Rdf_misc.opt_of_string (Rdf_misc.string_of_opt lang))
                ?typ value
            | _ ->
                let msg = "Bad result for term with hash \"" ^ (Int64.to_string hash) ^"\"" in
                raise (Error msg)
      end
  | _ ->
      let msg = "More than one term found with hash \"" ^ (Int64.to_string hash) ^ "\"" in
      raise (Error msg)
;;

let query_hash_list g stmt params =
  let res = Rdf_my.exec_prepared g.g_dbd stmt params in
  let f = function
  | [| Some hash |] -> Mysql.int642ml hash
  | _ -> raise (Error "Invalid result: NULL hash or too many fields")
  in
  Mysql.map res ~f
;;

let query_term_list g stmt params =
  let res = Rdf_my.exec_prepared g.g_dbd stmt params in
  let f = function
  | [| Some hash |] ->
      term_of_hash g.g_dbd (Mysql.int642ml hash)
  | _ -> raise (Error "Invalid result: NULL hash or too many fields")
  in
  Mysql.map res ~f
;;

let query_hash_triple_list g where_clause =
  let query =
    "SELECT subject, predicate, object FROM "^g.g_table^" where " ^ where_clause (* removed DISTINCT *)
  in
  let res = exec_query g.g_dbd query in
  let f = function
  | [| Some sub ; Some pred ; Some obj |] ->
      (Mysql.int642ml sub,
       Mysql.int642ml pred,
       Mysql.int642ml obj
      )
  | _ -> raise (Error "Invalid result: NULL hash(es) or bad number of fields")
  in
  Mysql.map res ~f
;;

let query_triple_list g where_clause =
  let query =
    "SELECT subject, predicate, object FROM "^g.g_table^" where " ^ where_clause (* removed DISTINCT *)
  in
  let res = exec_query g.g_dbd query in
  let f = function
  | [| Some sub ; Some pred ; Some obj |] ->
      (term_of_hash g.g_dbd (Mysql.int642ml sub),
       to_uri (term_of_hash g.g_dbd (Mysql.int642ml pred)),
       term_of_hash g.g_dbd (Mysql.int642ml obj)
      )
  | _ -> raise (Error "Invalid result: NULL hash(es) or bad number of fields")
  in
  Mysql.map res ~f
;;

let open_graph ?(options=[]) name =
  let db = db_of_options options in
  let engine =
    try List.assoc "engine" options
    with Not_found -> "InnoDB"
  in
  let engine = String.uppercase engine in
  let dbd = init_db db engine in
  let table_name = init_graph dbd engine name in
  { g_name = name ;
    g_table = table_name ;
    g_dbd = dbd ;
    g_in_transaction = false ;
    g_transactions = engine = "INNODB" ;
  }
;;

let add_triple g ~sub ~pred ~obj =
  let sub = hash_of_term g.g_dbd ~add:true sub in
  let pred = hash_of_term g.g_dbd ~add:true (Rdf_term.Uri pred) in
  let obj = hash_of_term g.g_dbd ~add:true obj in
  let params = [ Int64.to_string sub ; Int64.to_string pred ; Int64.to_string obj] in
  (* do not insert if already present *)
  let res = Rdf_my.exec_prepared g.g_dbd Rdf_my.prepared_count_triples params in
  match Mysql.fetch res with
    Some [| Some s |] when int_of_string s = 0 ->
      ignore(Rdf_my.exec_prepared g.g_dbd Rdf_my.prepared_insert_triple params)
  | _ -> ()
;;

let rem_triple g ~sub ~pred ~obj =
  let sub = hash_of_term g.g_dbd ~add:false sub in
  let pred = hash_of_term g.g_dbd ~add:false (Rdf_term.Uri pred) in
  let obj = hash_of_term g.g_dbd ~add:false obj in
  ignore(Rdf_my.exec_prepared g.g_dbd
   Rdf_my.prepared_delete_triple
   [ Int64.to_string sub; Int64.to_string pred; Int64.to_string obj]
  )
;;

let subjects_of g ~pred ~obj =
  query_term_list g Rdf_my.prepared_subjects_of
  [ Int64.to_string (hash_of_term g.g_dbd (Rdf_term.Uri pred)) ;
    Int64.to_string (hash_of_term g.g_dbd obj) ]
;;

let predicates_of g ~sub ~obj =
  List.map to_uri
    (query_term_list g Rdf_my.prepared_predicates_of
     [ Int64.to_string (hash_of_term g.g_dbd sub) ;
       Int64.to_string (hash_of_term g.g_dbd obj) ]
    )
;;

let objects_of g ~sub ~pred =
  query_term_list g Rdf_my.prepared_objects_of
  [ Int64.to_string (hash_of_term g.g_dbd sub) ;
    Int64.to_string (hash_of_term g.g_dbd (Rdf_term.Uri pred)) ]
;;

let mk_where_clause ?sub ?pred ?obj g =
  let mk_cond field = function
    None -> []
  | Some term ->
      [field ^"="^(Int64.to_string (hash_of_term g.g_dbd term))]
  in
  match sub, pred, obj with
    None, None, None -> "TRUE"
  | _ ->
      let pred_cond =
        match pred with
          None -> []
        | Some p -> ["predicate="^(Int64.to_string (hash_of_term g.g_dbd (Rdf_term.Uri p)))]
      in
      let l =
        (mk_cond "subject" sub) @
        pred_cond @
        (mk_cond "object" obj)
      in
      String.concat " AND " l
;;

let mk_hash_where_clause ?sub ?pred ?obj g =
  let mk_cond field = function
    None -> []
  | Some term ->
      [field ^"="^(Int64.to_string term)]
  in
  match sub, pred, obj with
    None, None, None -> "TRUE"
  | _ ->
      let l =
        (mk_cond "subject" sub) @
        (mk_cond "predicate" pred) @
        (mk_cond "object" obj)
      in
      String.concat " AND " l
;;

let find ?sub ?pred ?obj g =
  let clause = mk_where_clause ?sub ?pred ?obj g in
  query_triple_list g clause
;;

let exists = Rdf_my.exists mk_where_clause;;

let subjects g = query_term_list g Rdf_my.prepared_subject [];;
let predicates g = List.map to_uri (query_term_list g Rdf_my.prepared_predicate []);;
let objects g = query_term_list g Rdf_my.prepared_object [];;

module MyBGP =
  struct
    type term = Int64.t
    type g = t
    let term g t = hash_of_term g.g_dbd ~add: false t
    let rdfterm g t = term_of_hash g.g_dbd t
    let compare _ = Int64.compare
    let subjects g = query_hash_list g Rdf_my.prepared_subject []
    let objects g = query_hash_list g Rdf_my.prepared_object []
    let find ?sub ?pred ?obj g =
       let clause = mk_hash_where_clause ?sub ?pred ?obj g in
       query_hash_triple_list g clause
  end

module Mysql =
  struct
    let name = "mysql"
    type g = t
    type error = string
    exception Error = Error
    let string_of_error s = s

    let graph_name g = g.g_name
    let graph_size g = Rdf_my.graph_size g

    let open_graph = open_graph

    let add_triple = add_triple
    let rem_triple = rem_triple

    let add_triple_t g (sub, pred, obj) = add_triple g ~sub ~pred ~obj
    let rem_triple_t g (sub, pred, obj) = rem_triple g ~sub ~pred ~obj

    let subjects_of = subjects_of
    let predicates_of = predicates_of
    let objects_of = objects_of

    let find = find
    let exists = exists
    let exists_t (sub, pred, obj) g = exists ~sub ~pred ~obj g

    let subjects = subjects
    let predicates = predicates
    let objects = objects

    let transaction_start = Rdf_my.transaction_start
    let transaction_commit = Rdf_my.transaction_commit
    let transaction_rollback = Rdf_my.transaction_rollback

    let new_blank_id = Rdf_my.new_blank_id

    module BGP = MyBGP
  end;;

Rdf_graph.add_storage (module Mysql : Rdf_graph.Storage);;

(*
let output_times file map =
  let oc = open_out file in
  let total = SMap.fold
  (fun q (t,cpt) acc ->
     Printf.fprintf oc "%f;%f;%d;%s\n"
     (t /. (float cpt)) t cpt q;
     t +. acc
    )
    map 0.
  in
  Printf.fprintf oc "Total=%f\n" total;
  close_out oc
;;

let _ = Pervasives.at_exit
  (fun () ->
     output_times "rdf_my_prep_times.log" !prep_times;
     output_times "rdf_my_q_times.log" !q_times
  )
;;
*)
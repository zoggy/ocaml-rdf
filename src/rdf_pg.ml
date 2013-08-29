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

(** Postgresql storage. *)

module PG = Postgresql;;

open Rdf_term;;

let dbg = Rdf_misc.create_log_fun
  ~prefix: "Rdf_pg"
    "RDF_PG_DEBUG_LEVEL"
;;

type t =
  { g_name : Rdf_uri.uri ; (* graph name *)
    g_table : string ; (* name of the table with the statements *)
    g_dbd : PG.connection ;
    mutable g_in_transaction : bool ;
  }

type error = string
exception Error of string
let string_of_error s = s;;

let getvalue res t c =
  try res#getvalue t c
  with PG.Error e -> raise (Error (PG.string_of_error e))
;;

let getisnull res t c =
  try res#getisnull t c
  with PG.Error e -> raise (Error (PG.string_of_error e))
;;

let get_tuple res t =
  try res#get_tuple t
  with PG.Error e -> raise (Error (PG.string_of_error e))
;;

let quote_str s = "\"" ^ (String.escaped s) ^ "\"";;

let exec_query (dbd : PG.connection) q =
  dbg ~level: 3 (fun () -> "exec_query: " ^ q);
  let res =
    try dbd#exec q
    with PG.Error e -> raise (Error (PG.string_of_error e))
  in
  match res#status with
  | PG.Command_ok
  | PG.Tuples_ok -> res
  | PG.Copy_out | PG.Copy_in -> assert false
  | _ -> raise (Error res#error)
;;

let exec_prepared (dbd : PG.connection) stmt params =
  dbg ~level: 2 (fun () -> "exec_prepared: " ^ stmt);
  let query = "EXECUTE "^stmt^" " ^
    (match params with
       [] -> ""
     | _ -> "(" ^ (String.concat ", " params) ^ ")")
  in
  exec_query dbd query
;;

let connect options =
  let host = Rdf_misc.opt_of_string
    (Rdf_graph.get_option ~def: "" "host" options)
  in
  let dbname  = Rdf_misc.opt_of_string
    (Rdf_graph.get_option "database" options)
  in
  let port  = Rdf_misc.opt_of_string
    (Rdf_graph.get_option ~def: "" "port" options)
  in
  let password  = Rdf_misc.opt_of_string
    (Rdf_graph.get_option ~def: "" "password" options)
  in
  let user  = Rdf_misc.opt_of_string
    (Rdf_graph.get_option "user" options)
  in
  let c =
    try new PG.connection ?host ?port ?dbname ?user ?password ()
    with PG.Error e -> raise (Error (PG.string_of_error e))
  in
  match c#status with
    PG.Ok -> c
  | PG.Bad -> raise (Error "Connexion error")
;;

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
      let res = exec_query dbd test_query in
      match getvalue res 0 0 with
        s when int_of_string s = 0 ->
          let pre_query =
            match term with
              Uri uri ->
                "resources (id, value) values ("^
                  (Int64.to_string hash) ^", '" ^
                  (dbd#escape_string (Rdf_uri.string uri)) ^"')"
            | Literal lit ->
                "literals (id, value, language, datatype) values (" ^
                  (Int64.to_string hash) ^ ", '" ^
                  (dbd#escape_string lit.lit_value) ^"', '" ^
                  (dbd#escape_string (Rdf_misc.string_of_opt lit.lit_language)) ^ "', '" ^
                  (dbd#escape_string (Rdf_misc.string_of_opt (Rdf_misc.map_opt Rdf_uri.string lit.lit_type))) ^"')"
            | Blank_ id ->
                "bnodes (id, value) values (" ^
                  (Int64.to_string hash) ^", '" ^
                  (dbd#escape_string (Rdf_term.string_of_blank_id id)) ^ "')"
            | Blank -> assert false
          in
          let query = "INSERT INTO " ^ pre_query (* ON DUPLICATE KEY UPDATE value=value*) in
          ignore(exec_query dbd query)
      | _ -> ()
    end;
  hash
;;


let to_uri = function
  Rdf_term.Uri uri -> uri
| t -> failwith ("Not a URI:"^(Rdf_term.string_of_term t))
;;

let table_options = "WITHOUT OIDS";;
let creation_queries =
  [
    "CREATE TABLE IF NOT EXISTS graphs (id SERIAL, name text NOT NULL)" ;
    "CREATE TABLE IF NOT EXISTS bnodes (id bigint PRIMARY KEY NOT NULL, value text NOT NULL) " ;
    "CREATE TABLE IF NOT EXISTS resources (id bigint PRIMARY KEY NOT NULL, value text NOT NULL) ";
    "CREATE TABLE IF NOT EXISTS literals (id bigint PRIMARY KEY NOT NULL, value text NOT NULL,
                                          language text, datatype text) " ;
  ]
;;

let init_db options =
  let dbd = connect options in
  List.iter
  (fun q -> ignore (exec_query dbd (q^table_options))) creation_queries;
  dbd
;;

let graph_table_of_id id = "graph" ^ (string_of_int id);;

let rec graph_table_of_graph_name ?(first=true) (dbd : PG.connection) uri =
  let name = Rdf_uri.string uri in
  let query = "SELECT id FROM graphs WHERE name = '" ^ (dbd#escape_string name) ^ "'" in
  let res = exec_query dbd query in
  match res#ntuples with
  | 0 when not first ->
      let msg = "Could not get table name for graph '" ^ (dbd#escape_string name) ^ "'" in
      raise (Error msg)
  | 0 ->
      let query = "INSERT INTO graphs (name) VALUES ('" ^ (dbd#escape_string name) ^ "')" in
      ignore(exec_query dbd query);
      graph_table_of_graph_name ~first: false dbd uri
  | n ->
      let id = getvalue res 0 0 in
      graph_table_of_id (int_of_string id)
;;

let table_exists dbd table =
  let query = "SELECT 1 FROM " ^ table in
  try ignore(exec_query dbd query); true
  with Error _ -> false
;;

let prepared_term_of_hash = "term_of_hash";;
let prepared_count_triples = "count_triples";;
let prepared_insert_triple = "insert_triple";;
let prepared_delete_triple = "delete_triple";;
let prepared_subjects_of = "subjects_of";;
let prepared_predicates_of = "predicates_of";;
let prepared_objects_of = "objects_of";;
let prepared_subject = "subject" ;;
let prepared_predicate = "predicate";;
let prepared_object = "object";;
let prepared_cardinal = "cardinal";;

let make_select_term_list table col clause =
  "SELECT "^col^" FROM "^table^" where "^clause
;;

let prepare_query dbd name query =
  let q = "PREPARE "^name^" AS "^query in
   ignore(exec_query dbd q)
;;

let prepare_queries dbd table =
  dbg ~level: 1 (fun () -> "Preparing queries...");
  let query = "SELECT NULL, value, NULL, NULL, NULL FROM resources where id=$1 UNION ALL \
     SELECT NULL, NULL, value, language, datatype FROM literals where id=$2 UNION ALL \
     SELECT value, NULL, NULL, NULL, NULL FROM bnodes where id=$3 LIMIT 1"
  in
  prepare_query dbd prepared_term_of_hash query;


  let query = "SELECT COUNT(*) FROM "^table in
  prepare_query dbd prepared_cardinal query;

  let query =
    "SELECT COUNT(*) FROM "^table^" WHERE subject=$1 AND predicate=$2 AND object=$3"
  in
  prepare_query dbd prepared_count_triples query;

  let query =
    "INSERT INTO "^table^" (subject, predicate, object) VALUES ($1, $2, $3)"
  in
  prepare_query dbd prepared_insert_triple query;

  let query =
    "DELETE FROM "^table^" WHERE subject=$1 AND predicate=$2 AND object=$3"
  in
  prepare_query dbd prepared_delete_triple query;

  let query =
    let clause = "predicate=$1 AND object=$2" in
    make_select_term_list table "subject" clause
  in
  prepare_query dbd prepared_subjects_of query;

  let query =
    let clause = "subject=$1 AND object=$2" in
    make_select_term_list table "predicate" clause
  in
  prepare_query dbd prepared_predicates_of query;

  let query =
    let clause = "subject=$1 AND predicate=$2" in
    make_select_term_list table "object" clause
  in
  prepare_query dbd prepared_objects_of query;

  let query = "SELECT subject from " ^ table in
  prepare_query dbd prepared_subject query;

  let query = "SELECT predicate from " ^ table in
  prepare_query dbd prepared_predicate query;

  let query = "SELECT object from " ^ table in
  prepare_query dbd prepared_object query;
  dbg ~level: 1 (fun () -> "done")
;;

let init_graph dbd name =
  let table = graph_table_of_graph_name dbd name in
  if not (table_exists dbd table) then
    begin
      let query =
        "CREATE TABLE IF NOT EXISTS "^table^" (\
         subject bigint NOT NULL, predicate bigint NOT NULL, \
         object bigint NOT NULL) "^table_options
      in
      ignore(exec_query dbd query);
      let query =
        "CREATE INDEX ON "^table^" (subject, predicate, object)"
      in
      ignore(exec_query dbd query);
(*
      let query = Printf.sprintf
        "ALTER TABLE %s ADD UNIQUE INDEX (subject, predicate, object)" table
      in
      ignore(exec_query dbd query)
*)
    end;
  prepare_queries dbd table;
  table
;;

let term_of_hash dbd hash =
  let s_hash = Int64.to_string hash in
  let res = exec_prepared dbd prepared_term_of_hash [ s_hash ; s_hash ; s_hash ] in
  match res#ntuples with
  | 1 ->
      begin
        match getisnull res 0 0 with
          false ->
            let name = getvalue res 0 0 in
            Blank_ (Rdf_term.blank_id_of_string name)
        | true ->
            match getisnull res 0 1 with
              false ->
                let uri = getvalue res 0 1 in
                Rdf_term.term_of_uri_string uri
            | true ->
               match get_tuple res 0 with
                 [| _ ; _ ; value ; lang ; typ |] ->
                   let typ = Rdf_misc.map_opt
                      Rdf_uri.uri (Rdf_misc.opt_of_string typ)
                    in
                    Rdf_term.term_of_literal_string
                    ?lang: (Rdf_misc.opt_of_string lang)
                    ?typ value
                | _ ->
                    raise (Error "Bad field number in results")
      end
  | n when n < 1 ->
      let msg = "No term with hash \""^(Int64.to_string hash)^"\"" in
      raise (Error msg)
  | _ ->
      let msg = "More than one term found with hash \""^(Int64.to_string hash)^"\"" in
      raise (Error msg)
;;

let query_term_list g stmt params =
  let res = exec_prepared g.g_dbd stmt params in
  let size = res#ntuples in
  let rec iter n acc =
    if n < size then
      let acc = (term_of_hash g.g_dbd (Int64.of_string (getvalue res n 0))) :: acc in
      iter (n+1) acc
    else
      acc
  in
  iter 0 []
;;

let query_triple_list g where_clause =
  let query =
    "SELECT subject, predicate, object FROM "^g.g_table^" where "^where_clause (* removed DISTINCT *)
  in
  let res = exec_query g.g_dbd query in
  let size = res#ntuples in
  let rec iter n acc =
    if n < size then
      match get_tuple res n with
        [| sub ; pred ; obj |] ->
          let acc =
            (term_of_hash g.g_dbd (Int64.of_string sub),
             to_uri (term_of_hash g.g_dbd (Int64.of_string pred)),
             term_of_hash g.g_dbd (Int64.of_string obj)) :: acc
          in
          iter (n+1) acc
      | _ -> raise (Error "Invalid result: bad number of fields")
    else
      acc
  in
  iter 0 []
;;

let open_graph ?(options=[]) name =
  let dbd = init_db options in
  let table_name = init_graph dbd name in
  { g_name = name ;
    g_table = table_name ;
    g_dbd = dbd ;
    g_in_transaction = false ;
  }
;;

let add_triple g ~sub ~pred ~obj =
  let sub = hash_of_term g.g_dbd ~add:true sub in
  let pred = hash_of_term g.g_dbd ~add:true (Rdf_term.Uri pred) in
  let obj = hash_of_term g.g_dbd ~add:true obj in
  let params = [ Int64.to_string sub ; Int64.to_string pred; Int64.to_string obj] in
  (* do not insert if already present *)
  let res = exec_prepared g.g_dbd prepared_count_triples params in
  let s = getvalue res 0 0 in
  if int_of_string s <= 0 then
    ignore(exec_prepared g.g_dbd prepared_insert_triple params)
;;

let rem_triple g ~sub ~pred ~obj =
  let sub = hash_of_term g.g_dbd ~add:false sub in
  let pred = hash_of_term g.g_dbd ~add:false (Rdf_term.Uri pred) in
  let obj = hash_of_term g.g_dbd ~add:false obj in
  ignore(exec_prepared g.g_dbd
   prepared_delete_triple
   [ Int64.to_string sub; Int64.to_string pred; Int64.to_string obj]
  )
;;

let subjects_of g ~pred ~obj =
  query_term_list g prepared_subjects_of
  [ Int64.to_string (hash_of_term g.g_dbd (Rdf_term.Uri pred)) ;
    Int64.to_string (hash_of_term g.g_dbd obj) ]
;;

let predicates_of g ~sub ~obj =
  List.map to_uri
    (query_term_list g prepared_predicates_of
     [ Int64.to_string (hash_of_term g.g_dbd sub) ;
       Int64.to_string (hash_of_term g.g_dbd obj) ]
    )
;;

let objects_of g ~sub ~pred =
  query_term_list g prepared_objects_of
  [ Int64.to_string (hash_of_term g.g_dbd sub) ;
    Int64.to_string (hash_of_term g.g_dbd (Rdf_term.Uri pred)) ]
;;

let mk_where_clause ?sub ?pred ?obj g =
  let mk_cond field = function
    None -> []
  | Some term ->
      [field^"="^(Int64.to_string (hash_of_term g.g_dbd term))]
  in
  match sub, pred, obj with
    None, None, None -> "TRUE"
  | _ ->
      let pred_cond = match pred with
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

let find ?sub ?pred ?obj g =
  let clause = mk_where_clause ?sub ?pred ?obj g in
  query_triple_list g clause
;;


let graph_size g =
  let res = exec_prepared g.g_dbd prepared_cardinal [] in
  int_of_string (getvalue res 0 0)
;;

let exists ?sub ?pred ?obj g =
  let query = "SELECT COUNT(*) FROM %"^g.g_table^
    " where " ^ (mk_where_clause ?sub ?pred ?obj g)
  in
  let res = exec_query g.g_dbd query in
  let size = int_of_string (getvalue res 0 0) in
  size > 0
;;

let subjects g = query_term_list g prepared_subject [];;
let predicates g = List.map to_uri (query_term_list g prepared_predicate []);;
let objects g = query_term_list g prepared_object [];;

let transaction_start g =
  dbg ~level: 1 (fun () -> "Start transaction");
  if g.g_in_transaction then
    raise (Error "Already in a transaction. Nested transactions not allowed.");
  ignore(exec_query g.g_dbd "START TRANSACTION");
  g.g_in_transaction <- true
;;

let transaction_commit g =
  dbg ~level: 1 (fun () -> "Commit");
  if not g.g_in_transaction then
    raise (Error "Not in a transaction.");
  ignore(exec_query g.g_dbd "COMMIT");
  g.g_in_transaction <- false
;;

let transaction_rollback g =
 dbg ~level: 1 (fun () -> "Rollback");
 if not g.g_in_transaction then
    raise (Error "Not in a transaction.");
  ignore(exec_query g.g_dbd "ROLLBACK");
  g.g_in_transaction <- false
;;

let new_blank_id g =
  let cardinal =
    let query = "SELECT COUNT(*) FROM " ^ g.g_table in
    let res = exec_query g.g_dbd query in
    int_of_string (getvalue res 0 0)
  in
  let max_int = Int32.to_int (Int32.div Int32.max_int (Int32.of_int 2)) in
  Rdf_term.blank_id_of_string
    ("genid"^(string_of_int cardinal)^"-"^(string_of_int (Random.int max_int)))
;;

module Postgresql =
  struct
    let name = "postgresql"
    type g = t
    type error = string
    exception Error = Error
    let string_of_error s = s

    let graph_name g = g.g_name
    let graph_size = graph_size

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

    let transaction_start = transaction_start
    let transaction_commit = transaction_commit
    let transaction_rollback = transaction_rollback

    let new_blank_id = new_blank_id
  end;;

Rdf_graph.add_storage (module Postgresql : Rdf_graph.Storage);;




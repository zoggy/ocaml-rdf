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

(** MySQL storage. *)

open Rdf_term;;

let dbg = Rdf_misc.create_log_fun
  ~prefix: "Rdf_my"
    "RDF_MY_DEBUG_LEVEL"
;;

type t =
  { g_name : Rdf_uri.uri ; (* graph name *)
    g_table : string ; (* name of the table with the statements *)
    g_dbd : Mysql.dbd ;
    mutable g_in_transaction : bool ;
    g_transactions : bool ;
  }

type error = string
exception Error = Mysql.Error
let string_of_error s = s;;

module SMap = Map.Make (struct type t = string let compare = Pervasives.compare end);;

(*
let prep_times = ref SMap.empty;;
let q_times = ref SMap.empty;;
let get_time () = Unix.gettimeofday ();;

let add_query_time map q t =
  let (old, cpt) =
    try SMap.find q !map
    with Not_found -> (0., 0)
  in
  let t = old +. t in
  map := SMap.add q (t, cpt+1) !map
;;
*)

let exec_query dbd q =
  dbg ~level: 2 (fun () -> "exec_query: " ^ q);
(*
  let t_start = get_time () in
*)
  let res = Mysql.exec dbd q in
  match Mysql.status dbd with
    Mysql.StatusOK | Mysql.StatusEmpty ->
(*
      let t_stop = get_time () in
      add_query_time q_times (String.sub q 0 (min (String.length q) 40)) (t_stop -. t_start);
*)
      res
  | Mysql.StatusError _ ->
      let msg = Rdf_misc.string_of_opt (Mysql.errmsg dbd) in
      raise (Error msg)
;;

let exec_prepared dbd stmt params =
  dbg ~level: 2 (fun () -> "exec_prepared: " ^ stmt);
  let params = List.mapi
    (fun n p ->
      let v = "@p"^(string_of_int n) in
      let q = "SET " ^ v ^ " = " ^ p in
      ignore(exec_query dbd q);
      v)
    params
  in
(*
  let t_start = get_time () in
*)
  let query =
    "EXECUTE "
      ^ stmt
      ^
      (match params with
         [] -> ""
       | _ -> " USING " ^ (String.concat ", " params)
      )
  in
  let res = exec_query dbd query in
(*
  let t_stop = get_time () in
  add_query_time prep_times stmt (t_stop -. t_start);
*)
  res
;;

let db_of_options options =
  let dbhost = Rdf_misc.opt_of_string
    (Rdf_graph.get_option ~def: "" "host" options)
  in
  let dbname  = Rdf_misc.opt_of_string
    (Rdf_graph.get_option "database" options)
  in
  let dbport  = Rdf_misc.map_opt int_of_string
    (Rdf_misc.opt_of_string (Rdf_graph.get_option ~def: "" "port" options))
  in
  let dbpwd  = Rdf_misc.opt_of_string
    (Rdf_graph.get_option ~def: "" "password" options)
  in
  let dbuser  = Rdf_misc.opt_of_string
    (Rdf_graph.get_option "user" options)
  in
  { Mysql.dbhost = dbhost ; dbname ; dbport ; dbpwd ; dbuser ; dbsocket = None }

;;

let mysql_quote_dbd dbd s =  "\""^(Mysql.real_escape dbd s)^"\""
let mysql_quote g s =  mysql_quote_dbd g.g_dbd s

let init_db db creation_queries =
  let dbd = Mysql.connect db in
  List.iter
  (fun q -> ignore (exec_query dbd q)) creation_queries;
  dbd
;;

let graph_table_of_id id = "graph" ^ (string_of_int id);;

let rec graph_table_of_graph_name ?(first=true) dbd uri =
  let name = Rdf_uri.string uri in
  let query = "SELECT id FROM graphs WHERE name = " ^(mysql_quote_dbd dbd name) in
  let res = exec_query dbd query in
  match Mysql.fetch res with
    Some [| Some id |] -> graph_table_of_id (int_of_string id)
  | _ when not first ->
      let msg = Printf.sprintf "Could not get table name for graph %S" name in
      raise (Error msg)
  | _ ->
      let query = "INSERT INTO graphs (name) VALUES (" ^(mysql_quote_dbd dbd name)  ^ ")" in
      ignore(exec_query dbd query);
      graph_table_of_graph_name ~first: false dbd uri
;;

let table_exists dbd table =
  let query = "SELECT 1 FROM " ^ table in
  try ignore(exec_query dbd query); true
  with Error _ -> false
;;

let prepared_count_triples = "count_triples";;
let prepared_insert_triple = "insert_triple";;
let prepared_delete_triple = "delete_triple";;
let prepared_subjects_of = "subjects_of";;
let prepared_predicates_of = "predicates_of";;
let prepared_objects_of = "objects_of";;
let prepared_subject = "subject" ;;
let prepared_predicate = "predicate";;
let prepared_object = "object";;

let make_select_term_list table col clause =
  "SELECT "^col^" FROM "^table^" where "^clause
;;

let prepare_query dbd name query =
  let q = "PREPARE "^name^" FROM " ^ (mysql_quote_dbd dbd query) in
   ignore(exec_query dbd q)
;;

let prepare_queries dbd ?(more=[]) table =
  dbg ~level: 1 (fun () -> "Preparing queries...");

  List.iter (fun (name, q) -> prepare_query dbd name q) more;

  let query =
    "SELECT COUNT(*) FROM "^table^" WHERE subject=? AND predicate=? AND object=?"
  in
  prepare_query dbd prepared_count_triples query;

  let query =
    "INSERT INTO "^table^" (subject, predicate, object) VALUES (?, ?, ?)"
  in
  prepare_query dbd prepared_insert_triple query;

  let query =
    "DELETE FROM "^table^" WHERE subject=? AND predicate=? AND object=?"
  in
  prepare_query dbd prepared_delete_triple query;

  let query =
    let clause = "predicate=? AND object=?" in
    make_select_term_list table "subject" clause
  in
  prepare_query dbd prepared_subjects_of query;

  let query =
    let clause = "subject=? AND object=?" in
    make_select_term_list table "predicate" clause
  in
  prepare_query dbd prepared_predicates_of query;

  let query =
    let clause = "subject=? AND predicate=?" in
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

let exists mk_where_clause ?sub ?pred ?obj g =
  let query = "SELECT COUNT(*) FROM " ^ g.g_table ^
    " where "^ (mk_where_clause ?sub ?pred ?obj g)
  in
  let res = exec_query g.g_dbd query in
  match Mysql.fetch res with
    Some [| Some n |] -> int_of_string n > 0
  | _ ->
    let msg = "Bad result for query: " ^ query in
    raise (Error msg)
;;

let transaction_start g =
  if not g.g_transactions then
    raise (Error "Transactions not supported by this engine.");
  if g.g_in_transaction then
      raise (Error "Already in a transaction. Nested transactions not allowed.");
    ignore(exec_query g.g_dbd "START TRANSACTION");
  g.g_in_transaction <- true
;;

let transaction_commit g =
 if not g.g_in_transaction then
    raise (Error "Not in a transaction.");
  ignore(exec_query g.g_dbd "COMMIT");
  g.g_in_transaction <- false
;;

let transaction_rollback g =
 if not g.g_in_transaction then
    raise (Error "Not in a transaction.");
  ignore(exec_query g.g_dbd "ROLLBACK");
  g.g_in_transaction <- false
;;

let new_blank_id g =
  let cardinal =
    let query = "SELECT COUNT(*) FROM " ^ g.g_table in
    let res = exec_query g.g_dbd query in
    match Mysql.fetch res with
      Some [|Some s|] -> int_of_string s
    | _ -> 0
  in
  let max_int = Int32.to_int (Int32.div Int32.max_int (Int32.of_int 2)) in
  Rdf_term.blank_id_of_string
    ("genid"^(string_of_int cardinal) ^"-" ^ (string_of_int (Random.int max_int)))
;;

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
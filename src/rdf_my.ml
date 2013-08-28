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
let quote_str s = "\"" ^ (String.escaped s) ^ "\"";;

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

let init_db db engine =
  let dbd = Mysql.connect db in
  let table_options = table_options engine in
  List.iter
  (fun q -> ignore (exec_query dbd (q^table_options))) creation_queries;
  dbd
;;

let graph_table_of_id id = "graph" ^ (string_of_int id);;

let rec graph_table_of_graph_name ?(first=true) dbd uri =
  let name = Rdf_uri.string uri in
  let query = "SELECT id FROM graphs WHERE name = " ^(quote_str name) in
  let res = exec_query dbd query in
  match Mysql.fetch res with
    Some [| Some id |] -> graph_table_of_id (int_of_string id)
  | _ when not first ->
      let msg = Printf.sprintf "Could not get table name for graph %S" name in
      raise (Error msg)
  | _ ->
      let query = "INSERT INTO graphs (name) VALUES (" ^ (quote_str name) ^ ")" in
      ignore(exec_query dbd query);
      graph_table_of_graph_name ~first: false dbd uri
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

let make_select_term_list table col clause =
  "SELECT "^col^" FROM "^table^" where "^clause
;;

let to_uri = function
  Rdf_term.Uri uri -> uri
| t -> failwith ("Not a URI:"^(Rdf_term.string_of_term t))
;;

let prepare_query dbd name query =
  let q = "PREPARE "^name^" FROM " ^ (quote_str query) in
   ignore(exec_query dbd q)
;;

let prepare_queries dbd table =
  dbg ~level: 1 (fun () -> "Preparing queries...");
  let query = "SELECT NULL, value, NULL, NULL, NULL FROM resources where id=? LIMIT 1 UNION ALL \
     SELECT NULL, NULL, value, language, datatype FROM literals where id=? LIMIT 1 UNION ALL \
     SELECT value, NULL, NULL, NULL, NULL FROM bnodes where id=? LIMIT 1"
  in
  prepare_query dbd prepared_term_of_hash query;

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
let init_graph dbd engine name =
  let table = graph_table_of_graph_name dbd name in
  if not (table_exists dbd table) then
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

let query_term_list g stmt params =
  let res = exec_prepared g.g_dbd stmt params in
  let f = function
  | [| Some hash |] ->
      term_of_hash g.g_dbd (Mysql.int642ml hash)
  | _ -> raise (Error "Invalid result: NULL hash or too many fields")
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
  let dbd = init_db db engine in
  let table_name = init_graph dbd engine name in
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
  let params = [ Int64.to_string sub ; Int64.to_string pred ; Int64.to_string obj] in
  (* do not insert if already present *)
  let res = exec_prepared g.g_dbd prepared_count_triples params in
  match Mysql.fetch res with
    Some [| Some s |] when int_of_string s = 0 ->
      ignore(exec_prepared g.g_dbd prepared_insert_triple params)
  | _ -> ()
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

let find ?sub ?pred ?obj g =
  let clause = mk_where_clause ?sub ?pred ?obj g in
  query_triple_list g clause
;;

let exists ?sub ?pred ?obj g =
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

let subjects g = query_term_list g prepared_subject [];;
let predicates g = List.map to_uri (query_term_list g prepared_predicate []);;
let objects g = query_term_list g prepared_object [];;

let transaction_start g =
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

module Mysql =
  struct
    let name = "mysql"
    type g = t
    type error = string
    exception Error = Error
    let string_of_error s = s

    let graph_name g = g.g_name

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
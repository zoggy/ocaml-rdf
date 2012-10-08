(*********************************************************************************)
(*                OCaml-RDF                                                      *)
(*                                                                               *)
(*    Copyright (C) 2012 Institut National de Recherche en Informatique          *)
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

open Rdf_node;;

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

let exec_query (dbd : PG.connection) q =
  dbg ~level: 2 (fun () -> Printf.sprintf "exec_query: %s" q);
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

let hash_of_node dbd ?(add=false) node =
  let hash = Rdf_node.node_hash node in
  if add then
    begin
      let test_query = Printf.sprintf
        "SELECT COUNT(*) FROM %s WHERE id=%Ld"
        (match node with
           Uri _ -> "resources"
         | Literal _ -> "literals"
         | Blank_ _ | Blank -> "bnodes"
        )
        hash
      in
      let res = exec_query dbd test_query in
      match getvalue res 0 0 with
        s when int_of_string s = 0 ->
          let pre_query =
            match node with
              Uri uri ->
                Printf.sprintf "resources (id, value) values (%Ld, '%s')"
                hash (dbd#escape_string (Rdf_uri.string uri))
            | Literal lit ->
                Printf.sprintf
                "literals (id, value, language, datatype) \
                 values (%Ld, '%s', '%s', '%s')"
                hash
                (dbd#escape_string lit.lit_value)
                (dbd#escape_string (Rdf_misc.string_of_opt lit.lit_language))
                (dbd#escape_string (Rdf_misc.string_of_opt (Rdf_misc.map_opt Rdf_uri.string lit.lit_type)))
            | Blank_ id ->
                Printf.sprintf "bnodes (id, value) values (%Ld, '%s')"
                hash (dbd#escape_string (Rdf_node.string_of_blank_id id))
            | Blank -> assert false
          in
          let query = Printf.sprintf "INSERT INTO %s" pre_query (* ON DUPLICATE KEY UPDATE value=value*) in
          ignore(exec_query dbd query)
      | _ -> ()
    end;
  hash
;;


let table_options = "";;
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

let graph_table_of_id id = Printf.sprintf "graph%d" id;;

(* FIXME: cache this using a Urimap ? *)
let rec graph_table_of_graph_name ?(first=true) (dbd : PG.connection) uri =
  let name = Rdf_uri.string uri in
  let query = Printf.sprintf "SELECT id FROM graphs WHERE name = '%s'" (dbd#escape_string name) in
  let res = exec_query dbd query in
  match res#ntuples with
  | 0 when not first ->
      let msg = Printf.sprintf "Could not get table name for graph '%s'" (dbd#escape_string name) in
      raise (Error msg)
  | 0 ->
      let query = Printf.sprintf "INSERT INTO graphs (name) VALUES ('%s')" (dbd#escape_string name) in
      ignore(exec_query dbd query);
      graph_table_of_graph_name ~first: false dbd uri
  | n ->
      let id = getvalue res 0 0 in
      graph_table_of_id (int_of_string id)
;;

let table_exists dbd table =
  let query = Printf.sprintf "SELECT 1 FROM %s" table in
  try ignore(exec_query dbd query); true
  with Error _ -> false
;;

let init_graph dbd name =
  let table = graph_table_of_graph_name dbd name in
  if not (table_exists dbd table) then
    begin
      let query = Printf.sprintf
        "CREATE TABLE IF NOT EXISTS %s (\
         subject bigint NOT NULL, predicate bigint NOT NULL, \
         object bigint NOT NULL) %s"
        table table_options
      in
      ignore(exec_query dbd query);
(*
      let query = Printf.sprintf
        "ALTER TABLE %s ADD UNIQUE INDEX (subject, predicate, object)" table
      in
      ignore(exec_query dbd query)
*)
    end;
  table
;;

let node_of_hash dbd hash =
  let query = Printf.sprintf
    "SELECT NULL, value, NULL, NULL, NULL FROM resources where id=%Ld UNION \
     SELECT NULL, NULL, value, language, datatype FROM literals where id=%Ld UNION \
     SELECT value, NULL, NULL, NULL, NULL FROM bnodes where id=%Ld"
    hash hash hash
  in
  let res = exec_query dbd query in
  match res#ntuples with
  | 1 ->
      begin
        match getisnull res 0 0 with
          false ->
            let name = getvalue res 0 0 in
            Blank_ (Rdf_node.blank_id_of_string name)
        | true ->
            match getisnull res 0 1 with
              false ->
                let uri = getvalue res 0 1 in
                Rdf_node.node_of_uri_string uri
            | true ->
               match get_tuple res 0 with
                 [| _ ; _ ; value ; lang ; typ |] ->
                   let typ = Rdf_misc.map_opt
                      Rdf_uri.uri (Rdf_misc.opt_of_string typ)
                    in
                    Rdf_node.node_of_literal_string
                    ?lang: (Rdf_misc.opt_of_string lang)
                    ?typ value
                | _ ->
                    raise (Error "Bad field number in results")
      end
  | n when n < 1 ->
      let msg = Printf.sprintf "No node with hash \"%Ld\"" hash in
      raise (Error msg)
  | _ ->
      let msg = Printf.sprintf "More than one node found with hash \"%Ld\"" hash in
      raise (Error msg)
;;

let query_node_list g field where_clause =
  let query = Printf.sprintf "SELECT  %s FROM %s where %s" (* removed DISTINCT *)
    field g.g_table where_clause
  in
  let res = exec_query g.g_dbd query in
  let size = res#ntuples in
  let rec iter n acc =
    if n < size then
      let acc = (node_of_hash g.g_dbd (Int64.of_string (getvalue res n 0))) :: acc in
      iter (n+1) acc
    else
      acc
  in
  iter 0 []
;;

let query_triple_list g where_clause =
  let query = Printf.sprintf
    "SELECT subject, predicate, object FROM %s where %s" (* removed DISTINCT *)
    g.g_table where_clause
  in
  let res = exec_query g.g_dbd query in
  let size = res#ntuples in
  let rec iter n acc =
    if n < size then
      match get_tuple res n with
        [| sub ; pred ; obj |] ->
          let acc =
            (node_of_hash g.g_dbd (Int64.of_string sub),
             node_of_hash g.g_dbd (Int64.of_string pred),
             node_of_hash g.g_dbd (Int64.of_string obj)) :: acc
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
  let sub = hash_of_node g.g_dbd ~add:true sub in
  let pred = hash_of_node g.g_dbd ~add:true pred in
  let obj = hash_of_node g.g_dbd ~add:true obj in
  (* do not insert if already present *)
  let query = Printf.sprintf
    "SELECT COUNT(*) FROM %s WHERE subject=%Ld AND predicate=%Ld AND object=%Ld"
    g.g_table sub pred obj
  in
  let res = exec_query g.g_dbd query in
  let s = getvalue res 0 0 in
  if int_of_string s <= 0 then
    (
     let query = Printf.sprintf
       "INSERT INTO %s (subject, predicate, object) VALUES (%Ld, %Ld, %Ld)"
       g.g_table sub pred obj
     in
     ignore(exec_query g.g_dbd query)
    )
;;

let rem_triple g ~sub ~pred ~obj =
  let sub = hash_of_node g.g_dbd ~add:false sub in
  let pred = hash_of_node g.g_dbd ~add:false pred in
  let obj = hash_of_node g.g_dbd ~add:false obj in
  let query = Printf.sprintf
    "DELETE FROM %s WHERE subject=%Ld AND predicate=%Ld AND object=%Ld"
    g.g_table sub pred obj
  in
  ignore(exec_query g.g_dbd query)
;;

let subjects_of g ~pred ~obj =
  query_node_list g "subject"
  (Printf.sprintf "predicate=%Ld AND object=%Ld"
   (hash_of_node g.g_dbd pred) (hash_of_node g.g_dbd obj))
;;

let predicates_of g ~sub ~obj =
  query_node_list g "predicate"
  (Printf.sprintf "subject=%Ld AND object=%Ld"
   (hash_of_node g.g_dbd sub) (hash_of_node g.g_dbd obj))
;;

let objects_of g ~sub ~pred =
  query_node_list g "object"
  (Printf.sprintf "subject=%Ld AND predicate=%Ld"
   (hash_of_node g.g_dbd sub) (hash_of_node g.g_dbd pred))
;;

let mk_where_clause ?sub ?pred ?obj g =
  let mk_cond field = function
    None -> []
  | Some node ->
      [Printf.sprintf "%s=%Ld" field (hash_of_node g.g_dbd node)]
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

let exists ?sub ?pred ?obj g =
  let query = Printf.sprintf "SELECT COUNT(*) FROM %s where %s"
    g.g_table (mk_where_clause ?sub ?pred ?obj g)
  in
  let res = exec_query g.g_dbd query in
  let size = int_of_string (getvalue res 0 0) in
  size > 0
;;

let subjects g = query_node_list g "subject" "TRUE";;
let predicates g = query_node_list g "predicate" "TRUE";;
let objects g = query_node_list g "object" "TRUE";;

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
    let query = Printf.sprintf "SELECT COUNT(*) FROM %s" g.g_table in
    let res = exec_query g.g_dbd query in
    int_of_string (getvalue res 0 0)
  in
  let max_int = Int32.to_int (Int32.div Int32.max_int (Int32.of_int 2)) in
  Rdf_node.blank_id_of_string
  (Printf.sprintf "%d-%d" cardinal (Random.int max_int))
;;

module Postgresql =
  struct
    let name = "postgresql"
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

Rdf_graph.add_storage (module Postgresql : Rdf_graph.Storage);;




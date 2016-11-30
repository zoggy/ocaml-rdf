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

open Rdf_term;;
open Rdf_my;;

let dbg = Rdf_my.dbg;;

let table_options engine =
  " ENGINE="^engine^
    (if String.lowercase_ascii engine = "myisam" then " DELAY_KEY_WRITE=1" else "")^
    " MAX_ROWS=100000000 DEFAULT CHARSET=UTF8"
;;

let creation_queries =
  [
    "CREATE TABLE IF NOT EXISTS graphs (id integer AUTO_INCREMENT PRIMARY KEY NOT NULL, name text NOT NULL)" ;
  ]
;;

let hash_of_term = function
| Rdf_term.Iri iri -> "U"^(Iri.to_string iri)
| Rdf_term.Blank -> assert false
| Rdf_term.Blank_ id -> "B"^(Rdf_term.string_of_blank_id id)
| Rdf_term.Literal lit ->
    let iri = Rdf_misc.map_opt Iri.to_string lit.lit_type in
    "L"^(Marshal.to_string (lit.lit_value, lit.lit_language, iri) [])
;;

let term_of_hash dbd hash =
  match hash.[0] with
    'U' -> Rdf_term.Iri (Iri.of_string (String.sub hash 1 (String.length hash - 1)))
  | 'B' -> Rdf_term.Blank_ (Rdf_term.blank_id_of_string (String.sub hash 1 (String.length hash -1)))
  | 'L' ->
      let (v,lang,iri) = Marshal.from_string hash 1 in
      Rdf_term.Literal
        { Rdf_term.lit_value = v ;
          lit_language = lang ;
          lit_type = Rdf_misc.map_opt Iri.of_string iri ;
        }
  | c -> raise (Error ("Bad term header '"^(String.make 1 c)^"'"))
;;

let init_db db engine =
  let table_options = table_options engine in
  let queries = List.map (fun q -> q^table_options) creation_queries in
  Rdf_my.init_db db queries
;;

let init_graph dbd engine name =
  let table = Rdf_my.graph_table_of_graph_name dbd name in
  if not (Rdf_my.table_exists dbd table) then
    begin
      let query =
        "CREATE TABLE IF NOT EXISTS "^table^" (\
         subject text NOT NULL, predicate text NOT NULL, \
         object text NOT NULL,\
         KEY SubjectPredicate (subject(100),predicate(100)),\
         KEY PredicateObject (predicate(100),object(100)),\
         KEY SubjectObject (subject(100),object(100))\
        ) "^(table_options engine)^" AVG_ROW_LENGTH=250"
      in
      ignore(Rdf_my.exec_query dbd query);
    end;
  Rdf_my.create_namespaces_table dbd table ;
  Rdf_my.prepare_queries dbd table;
  table
;;

let query_term_list g stmt params =
  let res = Rdf_my.exec_prepared g.g_dbd stmt params in
  let f = function
  | [| Some hash |] -> term_of_hash g.g_dbd (Mysql.blob2ml hash)
  | _ -> raise (Error "Invalid result: NULL hash or too many fields")
  in
  Mysql.map res ~f
;;

let query_pred_list g stmt params =
  let res = Rdf_my.exec_prepared g.g_dbd stmt params in
  let f = function
  | [| Some s |] -> Iri.of_string (Mysql.blob2ml s)
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
      (term_of_hash g.g_dbd (Mysql.blob2ml sub),
       Iri.of_string (Mysql.blob2ml pred),
       term_of_hash g.g_dbd (Mysql.blob2ml obj)
      )
  | _ -> raise (Error "Invalid result: NULL hash(es) or bad number of fields")
  in
  Mysql.map res ~f
;;

let open_graph ?(options=[]) name =
  let db = db_of_options options in
  let engine =
    try
      match List.assoc "engine" options with
        "" -> raise Not_found
      | s -> s
    with Not_found -> "InnoDB"
  in
  let engine = String.uppercase_ascii engine in
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
  let sub = hash_of_term sub in
  let pred = Iri.to_string pred in
  let obj = hash_of_term obj in
  let params = [
    Rdf_my.mysql_quote g sub ;
    Rdf_my.mysql_quote g pred ;
    Rdf_my.mysql_quote g obj
    ]
  in
  (* do not insert if already present *)
  let res = Rdf_my.exec_prepared g.g_dbd prepared_count_triples params in
  match Mysql.fetch res with
    Some [| Some s |] when int_of_string s = 0 ->
      ignore(Rdf_my.exec_prepared g.g_dbd prepared_insert_triple params)
  | _ -> ()
;;

let rem_triple g ~sub ~pred ~obj =
  let sub = hash_of_term sub in
  let pred = Iri.to_string pred in
  let obj = hash_of_term obj in
  ignore(Rdf_my.exec_prepared g.g_dbd
   prepared_delete_triple
   [ Rdf_my.mysql_quote g sub;
     Rdf_my.mysql_quote g pred;
     Rdf_my.mysql_quote g obj
   ]
  )
;;

let subjects_of g ~pred ~obj =
  query_term_list g prepared_subjects_of
  [ Rdf_my.mysql_quote g (Iri.to_string pred) ;
    Rdf_my.mysql_quote g (hash_of_term obj) ;
  ]
;;

let predicates_of g ~sub ~obj =
  query_pred_list g prepared_predicates_of
   [ Rdf_my.mysql_quote g (hash_of_term sub) ;
     Rdf_my.mysql_quote g (hash_of_term obj) ;
   ]
;;

let objects_of g ~sub ~pred =
  query_term_list g prepared_objects_of
  [ Rdf_my.mysql_quote g (hash_of_term sub) ;
    Rdf_my.mysql_quote g (Iri.to_string pred) ;
  ]
;;

let mk_where_clause ?sub ?pred ?obj g =
  let mk_cond field = function
    None -> []
  | Some term ->
      [field ^"="^(Rdf_my.mysql_quote g (hash_of_term term))]
  in
  match sub, pred, obj with
    None, None, None -> "TRUE"
  | _ ->
      let pred_cond =
        match pred with
          None -> []
        | Some p -> ["predicate="^(Rdf_my.mysql_quote g (Iri.to_string p) )]
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

let exists = Rdf_my.exists mk_where_clause;;

let subjects g = query_term_list g prepared_subject [];;
let predicates g = query_pred_list g prepared_predicate [];;
let objects g = query_term_list g prepared_object [];;


module MyBGP =
  struct
    let to_iri (sub,pred,obj) = (sub, Rdf_term.Iri pred, obj)
    type term = Rdf_term.term
    type g = t
    let term _ t = t
    let rdfterm _ t = t
    let compare _ = Rdf_term.compare
    let subjects = subjects
    let objects = objects
    let find ?sub ?pred ?obj g =
      match pred with
        None -> List.map to_iri (find ?sub ?obj g)
      | Some (Rdf_term.Iri iri) ->
          List.map to_iri (find ?sub ~pred: iri ?obj g)
      | _ -> []
  end

module Mysql =
  struct
    let name = "mysql2"
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

    let namespaces = Rdf_my.namespaces
    let add_namespace = Rdf_my.add_namespace
    let rem_namespace = Rdf_my.rem_namespace
    let set_namespaces = Rdf_my.set_namespaces

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
(** MySQL storage. *)

open Rdf_types;;

let dbg = Rdf_misc.create_log_fun
  ~prefix: "Rdf_mysql"
    "RDF_MYSQL_DEBUG_LEVEL"
;;

type t =
  { g_name : uri ; (* graph name *)
    g_table : string ; (* name of the table with the statements *)
    g_dbd : Mysql.dbd ;
  }

type error = string
exception Error = Mysql.Error
let string_of_error s = s;;

let exec_query dbd q =
  dbg ~level: 2 (fun () -> Printf.sprintf "exec_query: %s" q);
  let res = Mysql.exec dbd q in
  match Mysql.status dbd with
    Mysql.StatusOK | Mysql.StatusEmpty -> res
  | Mysql.StatusError _ ->
      let msg = Rdf_misc.string_of_opt (Mysql.errmsg dbd) in
      raise (Error msg)
;;

let db_of_options options =
  let dbhost = Rdf_misc.opt_of_string
    (Rdf_types.get_option ~def: "" "host" options)
  in
  let dbname  = Rdf_misc.opt_of_string
    (Rdf_types.get_option "database" options)
  in
  let dbport  = Rdf_misc.map_opt int_of_string
    (Rdf_misc.opt_of_string (Rdf_types.get_option ~def: "" "port" options))
  in
  let dbpwd  = Rdf_misc.opt_of_string
    (Rdf_types.get_option ~def: "" "password" options)
  in
  let dbuser  = Rdf_misc.opt_of_string
    (Rdf_types.get_option "user" options)
  in
  { Mysql.dbhost = dbhost ; dbname ; dbport ; dbpwd ; dbuser }

;;

let int64_hash str =
  let digest = Digest.string str in
  (* use the same method as in librdf: use the 8 first bytes to
     get a 64 bits integer independant from the little/big endianness *)
  let hash = ref Int64.zero in
  for i = 0 to 7 do
    hash := Int64.add !hash (Int64.shift_left (Int64.of_int (Char.code digest.[i])) (i*8))
  done;
  !hash
;;

let node_hash = function
  Uri uri -> int64_hash (Printf.sprintf "R%s" (Rdf_types.string_of_uri uri))
| Literal lit ->
    int64_hash (Printf.sprintf "L%s<%s>%s"
     lit.lit_value
     (Rdf_misc.string_of_opt lit.lit_language)
     (Rdf_misc.string_of_opt (Rdf_misc.map_opt Rdf_types.string_of_uri lit.lit_type)))
| Blank -> assert false
| Blank_ id -> int64_hash (Printf.sprintf "B%s" id)
;;

let node_id dbd ~add node =
  let hash = node_hash node in
  if add then
    begin
      let pre_query =
        match node with
          Uri uri ->
            Printf.sprintf "resources (id, uri) values (%Lu, %S)" hash uri
        | Literal lit ->
            Printf.sprintf
            "literals (id, value, language, datatype) \
             values (%Lu, %S, %S, %S)"
            hash
            lit.lit_value
            (Rdf_misc.string_of_opt lit.lit_language)
            (Rdf_misc.string_of_opt (Rdf_misc.map_opt Rdf_types.string_of_uri lit.lit_type))
        | Blank_ id ->
            Printf.sprintf "bnodes (id, name) values (%Lu, %S)" hash id
        | Blank -> assert false
      in
      let query = Printf.sprintf "INSERT INTO %s ON DUPLICATE KEY UPDATE id=id" pre_query in
      ignore(exec_query dbd query)
    end;
  hash
;;


let table_options = " ENGINE=InnoDB DEFAULT CHARSET=UTF8";;
let creation_queries =
  [
    "CREATE TABLE IF NOT EXISTS graphs (id integer AUTO_INCREMENT PRIMARY KEY NOT NULL, name text NOT NULL)" ;
    "CREATE TABLE IF NOT EXISTS bnodes (id bigint unsigned PRIMARY KEY NOT NULL, name text NOT NULL)" ;
    "CREATE TABLE IF NOT EXISTS resources (id bigint unsigned PRIMARY KEY NOT NULL, uri text NOT NULL)" ;
    "CREATE TABLE IF NOT EXISTS literals (id bigint unsigned PRIMARY KEY NOT NULL, value longtext NOT NULL,
                                          language text NOT NULL, datatype text NOT NULL)" ;
  ]
;;

let init_db db =
  let dbd = Mysql.connect db in
  List.iter
  (fun q -> ignore (exec_query dbd (q^table_options))) creation_queries;
  dbd
;;

let graph_table_of_id id = Printf.sprintf "graph%d" id;;

let rec graph_table_of_graph_name ?(first=true) dbd name =
  let query = Printf.sprintf "SELECT id FROM graphs WHERE name = %S" name in
  let res = exec_query dbd query in
  match Mysql.fetch res with
    Some [| Some id |] -> graph_table_of_id (int_of_string id)
  | _ when not first ->
      let msg = Printf.sprintf "Could not get table name for graph %S" name in
      raise (Error msg)
  | _ ->
      let query = Printf.sprintf "INSERT INTO graphs (name) VALUES (%S)" name in
      ignore(exec_query dbd query);
      graph_table_of_graph_name ~first: false dbd name
;;

let init_graph dbd name =
  let table = graph_table_of_graph_name dbd name in
  let query = Printf.sprintf
    "CREATE TABLE IF NOT EXISTS %s (\
     subject bigint unsigned NOT NULL, predicate bigint unsigned NOT NULL, \
     object bigint unsigned NOT NULL)%s"
     table table_options
  in
  ignore(exec_query dbd query);
  let query = Printf.sprintf
     "ALTER TABLE %s ADD UNIQUE INDEX (subject, predicate, object)" table
  in
  ignore(exec_query dbd query);
  table
;;

let open_graph ?(options=[]) name =
  let db = db_of_options options in
  let dbd = init_db db in
  let table_name = init_graph dbd name in
  { g_name = name ;
    g_table = table_name ;
    g_dbd = dbd ;
  }
;;

let add_triple g ~sub ~pred ~obj =
  let sub = node_id g.g_dbd ~add:true sub in
  let pred = node_id g.g_dbd ~add:true pred in
  let obj = node_id g.g_dbd ~add:true obj in
  let query = Printf.sprintf
    "INSERT INTO %s (subject, predicate, object) VALUES (%Lu, %Lu, %Lu) ON DUPLICATE KEY UPDATE subject=subject"
    g.g_table sub pred obj
  in
  ignore(exec_query g.g_dbd query)
;;

let rem_triple g ~sub ~pred ~obj =
  let sub = node_id g.g_dbd ~add:false sub in
  let pred = node_id g.g_dbd ~add:false pred in
  let obj = node_id g.g_dbd ~add:false obj in
  let query = Printf.sprintf
    "DELETE FROM %s WHERE subject=%Lu AND predicate=%Lu AND object=%Lu"
    g.g_table sub pred obj
  in
  ignore(exec_query g.g_dbd query)
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
  end;;

Rdf_graph.add_storage (module Mysql : Rdf_graph.Storage);;




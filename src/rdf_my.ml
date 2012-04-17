(** MySQL storage. *)

open Rdf_types;;

type t =
  { g_name : uri ; (* graph name *)
    g_table : string ; (* name of the table with the statements *)
    g_dbd : Mysql.dbd ;
  }

type error = string
exception Error = Mysql.Error
let string_of_error s = s;;

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
    (Rdf_types.get_option "password" options)
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

let exec_query dbd q =
  let res = Mysql.exec dbd q in
  match Mysql.status dbd with
    Mysql.StatusOK | Mysql.StatusEmpty -> res
  | Mysql.StatusError _ ->
      let msg = Rdf_misc.string_of_opt (Mysql.errmsg dbd) in
      raise (Error msg)
;;

let creation_queries =
  [
    "CREATE TABLE IF NOT EXISTS graphs (id integer AUTO_INCREMENT NOT NULL, name text NOT NULL)" ;
    "CREATE TABLE IF NOT EXISTS bnodes (id bigint PRIMARY KEY NOT NULL, name text NOT NULL)" ;
    "CREATE TABLE IF NOT EXISTS resources (id bigint PRIMARY KEY NOT NULL, uri text NOT NULL)" ;
    "CREATE TABLE IF NOT EXISTS literals (id bigint PRIMARY KEY NOT NULL, value longtext NOT NULL,
                                          language text NOT NULL, datatype text NOT NULL)" ;
  ]
;;

let init_db db =
  let dbd = Mysql.connect db in
  List.iter (fun q -> ignore (exec_query dbd q)) creation_queries;
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
      let query = Printf.sprintf "INSERT INTO graphs COLUMNS (name) VALUES (%S)" name in
      ignore(exec_query dbd query);
      graph_table_of_graph_name ~first: false dbd name
;;

let init_graph dbd name =
  let table = graph_table_of_graph_name dbd name in
  let query = Printf.sprintf
    "CREATE TABLE IF NOT EXISTS %s (\
     SUBJECT bigint(20) unsigned NOT NULL, PREDICATE bigint(20) unsigned NOT NULL, \
     OBJECT bigint(20) unsigned NOT NULL)"
     table
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

module Mysql =
  struct
    let name = "mysql"
    type g = t
    type error = string
    exception Error = Error
    let string_of_error s = s

    let graph_name g = g.g_name

    let open_graph = open_graph
  end;;

Rdf_graph.add_storage (module Mysql : Rdf_graph.Storage);;




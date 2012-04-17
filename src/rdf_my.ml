(** MySQL storage. *)

open Rdf_types;;

type g =
  { g_name : uri ; (* graph name *)
    g_dbd : Mysql.dbd ;
  }

let graph_name g = g.g_name;;

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

let open_graph ?(options=[]) name =
  let db = db_of_options options in
  ignore(db)
;;



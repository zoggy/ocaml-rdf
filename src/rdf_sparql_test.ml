(** Testing. *)

module C = Config_file;;

type test_spec =
  { title : string ;
    desc : string ;
    query : string ;
    default_graph : string ;
    named : (Rdf_uri.uri * string) list ;
  }

let load_file file =
  let group = new C.group in
  let title = new C.string_cp ~group ["title"] "" "Title of the test" in
  let desc = new C.string_cp ~group ["descr"] "" "Description of the test" in
  let query = new C.string_cp ~group ["query-file"] "" "File containing the query" in
  let default_graph = new C.string_cp ~group ["default-graph"] "" "File containing the default graph" in
  let named = new C.list_cp (C.tuple2_wrappers C.string_wrappers C.string_wrappers)
    [ "named-graphs"] [] "Named graphs in the form (uri, file.ttl)"
  in
  group#read file

;;
  
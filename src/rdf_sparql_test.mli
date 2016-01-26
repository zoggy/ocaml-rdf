module C = Config_file
val verb : string -> unit
type test_spec = {
  base : Iri.t option;
  title : string;
  desc : string option;
  query : string;
  default_graph : string option;
  named : (Iri.t * string) list;
  options : (string * string) list;
}
type result = Error of string | Ok of Rdf_sparql.query_result
type test = { spec : test_spec; result : result; }
val load_file : ?graph_options:string -> string -> test_spec
val load_ttl : Rdf_graph.graph -> Iri.t -> string -> unit
val mk_dataset : test_spec -> Rdf_ds.dataset
val print_solution : Rdf_sparql.solution -> unit
val print_result : result -> unit

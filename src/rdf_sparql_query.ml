(** *)

open Rdf_dt
open Rdf_sparql_types
open Rdf_sparql_algebra

let dbg = Rdf_misc.create_log_fun
  ~prefix: "Rdf_sparql_eval"
    "RDF_SPARQL_QUERY_DEBUG_LEVEL"
;;


type result =
  Bool of bool
| Solutions of Rdf_sparql_ms.mu list
| Graph of Rdf_graph.graph

let execute ~base dataset query =
  let (base, ds, query) = Rdf_sparql_expand.expand_query base query in
  let q =
    match query.q_kind with
      Select s ->
        { Rdf_sparql_algebra.query_proj = Some s.select_select ;
          query_where = s.select_where ;
          query_modifier = s.select_modifier ;
          query_values = None ;
        }
    | Ask a ->
        { Rdf_sparql_algebra.query_proj = None ;
          query_where = a.ask_where ;
          query_modifier = a.ask_modifier ;
          query_values = None ;
        }
    | Construct c ->
        let w =
          match c.constr_where with
            Constr_ggp ggp -> ggp
          | Constr_template _ -> assert false (* FIXME: implement *)
        in
        { Rdf_sparql_algebra.query_proj = None ;
          query_where = w ;
          query_modifier = c.constr_modifier ;
          query_values = None ;
        }
    | Describe d ->
        let w =
          match d.desc_where with
          | None -> GGPSub { ggp_sub_loc = Rdf_loc.dummy_loc ; ggp_sub_elts = [] }
          | Some w -> w
        in
        { Rdf_sparql_algebra.query_proj = None ; (* FIXME: handle desc_sel *)
          query_where = w ;
          query_modifier = d.desc_modifier ;
          query_values = None ;
        }
  in
  let algebra = Rdf_sparql_algebra.translate_query_level q in
  dbg ~level: 2 (fun () -> Rdf_sparql_algebra.string_of_algebra algebra);
  dbg ~level: 2 (fun () -> Rdf_ttl.to_string dataset.Rdf_ds.default);
  let ctx = Rdf_sparql_eval.context ~base
    ?from: ds.Rdf_sparql_expand.from
        ~from_named: ds.Rdf_sparql_expand.from_named dataset
  in
  let solutions = Rdf_sparql_eval.eval_list ctx algebra in
  match query.q_kind with
    Select _ -> Solutions solutions
  | Ask _ -> Bool (solutions <> [])
  | Construct _ -> assert false
  | Describe _ -> assert false
;;


  
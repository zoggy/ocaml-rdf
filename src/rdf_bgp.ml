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


let dbg = Rdf_misc.create_log_fun
  ~prefix: "Rdf_bgp"
    "RDF_BGP_DEBUG_LEVEL"
;;

open Rdf_sparql_types;;
module A = Rdf_sparql_algebra;;

module type P =
  sig
    type g
    type term
    val term : Rdf_term.term -> term
    val compare : term -> term -> int
    val rdfterm : term -> Rdf_term.term
    val subjects : unit -> term list
    val objects : unit -> term list
    val find : ?sub: term -> ?pred: term -> ?obj: term -> unit ->
      (term * term * term) list
  end
;;


module type S =
  sig
    val eval_bgp :
      Rdf_sparql_algebra.triple list -> Rdf_sparql_ms.Multimu.t
  end

module Make (P : P) =
  struct
    module Mu = Rdf_sparql_types.SMap
    module TSet = Set.Make (struct type t = P.term let compare = P.compare end)

      (** Score of pattern selectivity, copied from
         https://bitbucket.org/dotnetrdf/dotnetrdf/wiki/DeveloperGuide/SPARQL/SPARQL%20Optimization. *)
    let triple_constraint_score =
      let of_var mu name =
        try ignore(Mu.find name mu); true
        with Not_found -> false
      in
      let of_node mu = function
        Var v -> of_var mu v.var_name
      | GraphTerm t ->
          match t with
            GraphTermBlank { bnode_label = Some s } -> of_var mu ("?"^s)
          | GraphTermBlank _ -> false
          | GraphTermNil -> false
          | GraphTermIri _
          | GraphTermLit _
          | GraphTermNumeric _
          | GraphTermBoolean _ -> true
          | GraphTermNode _ -> assert false
      in
      let rec of_path mu = function
      | A.Var v -> of_var mu v.var_name
      | A.Iri _ -> true
      | A.Inv p -> of_path mu p
      | _ -> false
      in
      fun mu (sub, path, obj) ->
        let sub = of_node mu sub in
        let pred = of_path mu path in
        let obj = of_node mu obj in
        match (sub, pred, obj) with
          true, true, true -> 7
        | true, true, false -> 6
        | true, false, true -> 4
        | false, true, true -> 4
        | true, false, false -> 3
        | false, true, false -> 2
        | false, false, true -> 1
        | false, false, false -> 0


    let triple_vars =
      let add = Rdf_sparql_types.SSet.add in
      let rec of_path set = function
        A.Var v -> add v.var_name set
      | A.Iri _ -> set
      | A.Inv p -> of_path set p
      | A.Alt (p1, p2) -> of_path (of_path set p1) p2
      | A.Seq (p1, p2) -> of_path (of_path set p1) p2
      | A.ZeroOrMore p -> of_path set p
      | A.OneOrMore p -> of_path set p
      | A.ZeroOrOne p -> of_path set p
      | A.NPS _ -> set
      in
      let of_node set = function
        Var v -> add v.var_name set
      | GraphTerm (GraphTermBlank { bnode_label = Some s }) -> add ("?"^s) set
      | _ -> set
      in
      fun (x,path,y) ->
        of_node (of_path (of_node Rdf_sparql_types.SSet.empty y) path) x


    let sort_triples =
      let add_data mu triple =
        (triple, triple_constraint_score mu triple, triple_vars triple)
      in
      let sort (t1,sc1,vars1) (t2,sc2,vars2) =
        (* sort: the most constrained first *)
        match sc2 - sc1 with
          0 ->
            (* then sort by variables used in triples*)
            Rdf_sparql_types.SSet.compare vars1 vars2
        | n -> n
      in
      let proj (t,s,_) = t in
      fun mu l ->
        match l with
          [] | [_] -> l
        | _ ->
            let l = List.map (add_data mu) l in
            List.map proj (List.sort sort l)


    let filter_of_var_or_term mu = function
      Rdf_sparql_types.Var v ->
        begin
          try
            let term = Mu.find v.var_name mu in
            (None, Some term)
          with Not_found -> (Some v.var_name, None)
        end
    | GraphTerm t ->
        match t with
          GraphTermIri (Iri iri) -> (None, Some (P.term (Rdf_term.Iri iri.iri_iri)))
        | GraphTermIri (PrefixedName _) -> assert false
        | GraphTermIri (Iriref _) -> assert false
        | GraphTermLit lit
        | GraphTermNumeric lit
        | GraphTermBoolean lit -> (None, Some (P.term (Rdf_term.Literal lit.rdf_lit)))
        | GraphTermBlank bn ->
            begin
              match bn.bnode_label with
                None -> (None, None)
              | Some s ->
                  let var_name = "?"^s in
                  try
                    let term = Mu.find var_name mu in
                    (*prerr_endline ("blank("^var_name^")=>"^(Rdf_term.string_of_term (P.rdfterm term)));*)
                    (None, Some term)
                  with Not_found -> (Some var_name, None)
            end
        | GraphTermNil -> (None, None)
        | GraphTermNode node -> (None, Some (P.term node))

    let eval_simple_triple =
      let add mu term = function
        None -> mu
      | Some name -> Mu.add name term mu
      in
      fun mu x path y ->
        dbg ~level: 2
          (fun () ->
             "eval_simple_triple "^
               (Rdf_sparql_algebra.string_of_triple (x, path, y))
          );
        let (vx, sub) = filter_of_var_or_term mu x in
        let (vy, obj) = filter_of_var_or_term mu y in
        let (vp, pred) =
          match path with
            A.Var v ->
              begin
                try
                  let term = Mu.find v.var_name mu in
                  (None, Some term)
                with Not_found ->
                    (Some v.var_name, None)
              end
          | A.Iri iri -> (None, Some (P.term (Rdf_term.Iri iri.iri_iri)))
          | _ -> assert false
        in
        let f acc (s,p,o) =
          dbg ~level: 3
            (fun () ->
               "simple_triple__f("^
                 (Rdf_term.string_of_term (P.rdfterm s))^", "^
                 (Rdf_term.string_of_term (P.rdfterm p))^", "^
                 (Rdf_term.string_of_term (P.rdfterm o))^")"
            );
          let mu = add mu s vx in
          let mu = add mu p vp in
          let mu = add mu o vy in
          mu :: acc
        in
        (* FIXME: we will use a fold in the graph when it is implemented *)
        let matches = P.find ?sub ?pred ?obj () in
        List.fold_left f [] matches


    let active_graph_subjects_and_objects () =
      let add set node = TSet.add node set in
      let set = List.fold_left add TSet.empty (P.subjects ()) in
      List.fold_left add set (P.objects ())

    let add_if_not_present mu ms =
      (* FIXME: List may not be the best datastructure... *)
      let pred mu2 = Mu.compare P.compare mu mu2 = 0 in
      if List.exists pred ms then ms else mu :: ms

    let rec eval_triples =
      let rec iter_join triples acc_mus mu =
        let triples = sort_triples mu triples in
        (*(match triples with
           [] -> ()
         | _ ->
             prerr_endline "sorted triples:";
             List.iter (fun t ->
                prerr_endline (Rdf_sparql_algebra.string_of_triple t))
               triples
        );*)
        (*prerr_endline ("iter_join triples="^(string_of_int (List.length triples)));
           prerr_endline ("acc_mus: "^(string_of_int (List.length acc_mus)));*)
        match triples with
          [] ->
            (*prerr_endline "concat";*)
            mu :: acc_mus
        | triple :: q ->
            let mus = eval_triple mu triple in
            List.fold_left (iter_join q) acc_mus mus
      in
      function
        | [] -> [ Mu.empty ]
        | triples -> iter_join triples [] Mu.empty

    and eval_triple_path_zero_or_one mu x p y =
            let ms_one = eval_simple_triple mu x p y in
            match x, y with
            | Var v, ((GraphTerm _) as t)
            | ((GraphTerm _) as t), Var v ->
                (*
                   eval(Path(X:term, ZeroOrOnePath(P), Y:var)) = { (Y, yn) | yn = X or {(Y, yn)} in eval(Path(X,P,Y)) }
                   eval(Path(X:var, ZeroOrOnePath(P), Y:term)) = { (X, xn) | xn = Y or {(X, xn)} in eval(Path(X,P,Y)) }
                   *)
                let (_, term) = filter_of_var_or_term mu t in
                (
                 match term with
                   None -> ms_one
                 | Some term ->
                     let pred mu =
                       try P.compare (Mu.find v.var_name mu) term = 0
                       with Not_found -> false
                     in
                     if List.exists pred ms_one then
                       ms_one
                     else
                       (
                        let mu = Mu.singleton v.var_name term in
                        mu :: ms_one
                       )
                )
            | GraphTerm _, GraphTerm _ ->
                (*
                   eval(Path(X:term, ZeroOrOnePath(P), Y:term)) =
                   { {} } if X = Y or eval(Path(X,P,Y)) is not empty
                   { } othewise
                   *)
                let (_, term1) = filter_of_var_or_term mu x in
                let (_, term2) = filter_of_var_or_term mu y in
                if not (ms_one = []) ||
                  Rdf_misc.opt_compare P.compare term1 term2 = 0
                then
                  [ Mu.empty ]
                else
                  []

            | Var v1, Var v2 ->
                (*
                   eval(Path(X:var, ZeroOrOnePath(P), Y:var)) =
                   { (X, xn) (Y, yn) | either (yn in nodes(G) and xn = yn) or {(X,xn), (Y,yn)} in eval(Path(X,P,Y)) }
                   *)
                let all_sub_and_obj = active_graph_subjects_and_objects () in
                let f term ms =
                  let mu = Mu.singleton v1.var_name term in
                  let mu = Mu.add v2.var_name term mu in
                  add_if_not_present mu ms
                in
                TSet.fold f all_sub_and_obj ms_one

    and eval_reachable =
            let term_of_graphterm mu0 t =
              match filter_of_var_or_term mu0 t with
                (_, None) -> assert false
              | (_, Some term) -> term
            in
            let rec iter mu0 gterm path var (seen, acc_ms) =
              let term = term_of_graphterm mu0 gterm in
              match TSet.mem term seen with
                true -> (seen, acc_ms)
              | false ->
                  let seen = TSet.add term seen in
                  let ms = eval_triple mu0 (gterm, path, Rdf_sparql_types.Var var) in
                  (* for each solution, use the term associated to var as
                     starting point for next iteration *)
                  let f (seen, acc_ms) mu =
                    try
                      let acc_ms = add_if_not_present mu acc_ms in
                      let node = P.rdfterm (Mu.find var.var_name mu) in
                      iter mu0 (GraphTerm (GraphTermNode node)) path var (seen, acc_ms)
                    with Not_found -> (seen, acc_ms)
                  in
                  List.fold_left f (seen, acc_ms) ms
            in
            fun ?(zero=false) mu0 term path var ->
              (*dbg ~level: 2
                (fun () ->
                   "eval_reachable card(mu0)="^(string_of_int (SMap.cardinal mu0))^
                     " term="^(Rdf_term.string_of_term (P.rdfterm (term_of_graphterm mu0 term)))^
                     ", var="^var.var_name^"\nmu="^
                     (String.concat ", "
                      (SMap.fold
                       (fun key v acc ->
                          (Printf.sprintf "%s=%s" key (Rdf_term.string_of_term (P.rdfterm v)))::acc)
                         mu0 []
                      )
                     )
                );
              *)
              let (_,ms) =
                let ms_start =
                  if zero then
                    [ Mu.singleton var.var_name (term_of_graphterm mu0 term) ]
                  else
                    []
                in
                iter mu0 term path var (TSet.empty, ms_start)
              in
              ms

    and eval_triple_path_or_more mu ~zero x p y =
      match x, y with
      | GraphTerm _, Var v ->
          (*dbg ~level: 2 (fun () -> "eval_triple_path_or_more GraphTerm-V");*)
          eval_reachable ~zero mu x p v
      | Var v, GraphTerm _ ->
          (*dbg ~level: 2 (fun () -> "eval_triple_path_or_more V-GraphTerm");*)
          begin
            let p = match p with A.Inv p -> p | p -> A.Inv p in
            eval_reachable ~zero mu y p v
          end
      | GraphTerm _, GraphTerm _ ->
          (*dbg ~level: 2 (fun () -> "eval_triple_path_or_more GraphTerm-GraphTerm");*)
          let term =
            match filter_of_var_or_term mu y with
              (_, None) ->  assert false
            | (_, Some term) -> term
          in
          let v = { var_loc = Rdf_loc.dummy_loc ; var_name = "__"^(Rdf_sparql_ms.gen_blank_id()) } in
          let solutions = eval_reachable ~zero mu x p v in
          let pred mu =
            try P.compare (Mu.find v.var_name mu) term = 0
            with Not_found -> false
          in
          if List.exists pred solutions then
            [ Mu.empty ]
          else
            []

      | Var v1, Var v2 ->
          (*dbg ~level: 2 (fun () -> "eval_triple_path_or_more V-V");*)
          let all_sub_and_obj = active_graph_subjects_and_objects () in
          (*dbg ~level: 2 (fun () -> "card(all_sub_and_obj)="^(string_of_int (TSet.cardinal all_sub_and_obj)));*)
          let f term acc_ms =
            let ms = eval_reachable ~zero mu
              (GraphTerm (GraphTermNode (P.rdfterm term))) p v2
            in
            (*dbg ~level:2
              (fun () -> "eval_triple_path_or_more (Var "^v1.var_name^") _ (Var "^v2.var_name^") card(ms)="^(string_of_int (List.length ms)));
            *)
            (* add (v1 -> term) to each returned solution *)
            let f acc_ms mu =
              let mu = Mu.add v1.var_name term mu in
              mu :: acc_ms
            in
            List.fold_left f acc_ms ms
          in
          TSet.fold f all_sub_and_obj []

    and eval_triple_path_nps mu x iris y =
      (* compute the triples and remove solutions where the predicate
         is one of the iris. *)
          let forbidden = List.fold_left
            (fun set iri -> TSet.add (P.term (Rdf_term.Iri iri.iri_iri)) set)
              TSet.empty iris
          in
          (* we use a dummy variable to access the predicate in each solution *)
          let v = { var_loc = Rdf_loc.dummy_loc ; var_name = "__"^(Rdf_sparql_ms.gen_blank_id()) } in
          let ms = eval_simple_triple mu x (A.Var v) y in
          let pred mu =
            try
              let p = Mu.find v.var_name mu in
              not (TSet.mem p forbidden)
            with Not_found -> false
          in
          List.filter pred ms

            (* See http://www.w3.org/TR/sparql11-query/#PropertyPathPatterns *)
    and eval_triple mu (x, path, y) =
      match path with
        A.Var _
      | A.Iri _ -> eval_simple_triple mu x path y
      | A.Inv p -> eval_triple mu (y, p, x)
      | A.Seq (p1, p2) ->
          let blank =
            let id = Rdf_sparql_ms.gen_blank_id () in
            GraphTerm
              (GraphTermBlank
               { bnode_loc = Rdf_loc.dummy_loc ;
                 bnode_label = Some id ;
               })
          in
          eval_triples [ (x, p1, blank) ; (blank, p2, y) ]
      | A.Alt (p1, p2) ->
          let o1 = eval_triples [ (x, p1, y) ] in
          let o2 = eval_triples [ (x, p2, y) ] in
          List.rev_append o1 o2
            (*let bgp1 = BGP [ (x, p1, y) ] in
               let bgp2 = BGP [ (x, p2, y) ] in
               eval (Union (bgp1, bgp2))*)
      | A.ZeroOrOne p ->
          eval_triple_path_zero_or_one mu x p y
      | A.ZeroOrMore p ->
          eval_triple_path_or_more ~zero: true mu x p y
      | A.OneOrMore p ->
          (* dbg ~level: 2 (fun () -> "OneOrMore");*)
          let ms = eval_triple_path_or_more ~zero: false mu x p y in
          (*dbg ~level: 2
            (fun () ->
               "OneOrMore: ms="^
               (String.concat "\n"
                  (List.map
                    (fun mu ->
                       (String.concat ", "
                        (SMap.fold
                         (fun key v acc ->
                            (Printf.sprintf "%s=%s" key (Rdf_term.string_of_term (P.rdfterm v)))::acc)
                           mu []
                        )
                       )
                    )
                    ms)
                 )
            );
             *)
          ms
      | A.NPS iris ->
          eval_triple_path_nps mu x iris y

   let eval_bgp =
      let graphmu_to_mu mu =
        Mu.fold
          (fun v term mu -> Rdf_sparql_ms.mu_add v (P.rdfterm term) mu)
          mu Rdf_sparql_ms.mu_0
      in
      fun triples ->
        (*prerr_endline ("triples: "^(string_of_int (List.length triples)));*)
        let mus = eval_triples triples in
        List.fold_left (fun o mu -> Rdf_sparql_ms.omega_add (graphmu_to_mu mu) o)
          Rdf_sparql_ms.Multimu.empty mus

end

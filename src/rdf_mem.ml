(*********************************************************************************)
(*                OCaml-RDF                                                      *)
(*                                                                               *)
(*    Copyright (C) 2012-2014 Institut National de Recherche en Informatique     *)
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

(** Memory storage, using tree-based sets. *)

let dbg = Rdf_misc.create_log_fun
  ~prefix: "Rdf_mem"
    "RDF_MEM_DEBUG_LEVEL"
;;

open Rdf_term;;
module SMap = Rdf_types.SMap;;

module Triples = functor (Map1 : Map.S) ->
  functor (Map2 : Map.S) ->
  functor (Set : Set.S) ->
  struct
    module Set = Set
    module Map = Map1
    type t = Set.t Map2.t Map1.t
    let empty = Map1.empty

    let add t x y z =
      let m =
        try Map1.find x t
        with Not_found -> Map2.empty
      in
      let set =
        try Set.add z (Map2.find y m)
        with Not_found -> Set.singleton z
      in
      let m = Map2.add y set m in
      Map1.add x m t

    let rem t x y z =
      let m =
        try Map1.find x t
        with Not_found -> Map2.empty
      in
      try
        let set = Set.remove z (Map2.find y m) in
        let m = Map2.add y set m in
        Map1.add x m t
      with
        Not_found -> t

    let find t x y =
      try Map2.find y (Map1.find x t)
      with Not_found -> Set.empty

    let find_list t x y = Set.elements (find t x y)

    let find2_list t x z =
      let f y set acc =
        if Set.mem z set then y :: acc else acc
      in
      try
        let m = Map1.find x t in
        Map2.fold f m []
      with Not_found -> []


    let triples_y x y set acc =
      let fz x y z acc = (x,y,z) :: acc in
      Set.fold (fz x y) set acc

    let triples_x t x acc =
      try Map2.fold (triples_y x) (Map1.find x t) acc
      with Not_found -> acc

    let triples =
      let fx elt map acc =
        Map2.fold (triples_y elt) map acc
      in
      fun t -> Map1.fold fx t []

    let x_list =
      let pred _ set = not (Set.is_empty set) in
      let fx elt map acc =
        if Map2.exists pred map then elt :: acc else acc
      in
      fun t -> Map1.fold fx t []

    let cardinal =
      let f_map2 _ set acc = acc + Set.cardinal set in
      let f_map _ map acc = Map2.fold f_map2 map acc in
      fun t -> Map1.fold f_map t 0

  end
;;
module Triples_s_p = Triples(Rdf_term.TMap)(Rdf_iri.Irimap)(Rdf_term.TSet);;
module Triples_p_o = Triples(Rdf_iri.Irimap)(Rdf_term.TMap)(Rdf_term.TSet);;
module Triples_o_s = Triples(Rdf_term.TMap)(Rdf_term.TMap)(Rdf_iri.Iriset);;

type t =
  { g_name : Rdf_iri.iri ; (* graph name *)
    mutable g_set_sub : Triples_s_p.t ; (* sub -> (pred -> obj set) *)
    mutable g_set_pred : Triples_p_o.t ; (* pred -> (obj -> sub set) *)
    mutable g_set_obj : Triples_o_s.t ; (* obj -> (sub -> pred set) *)
    mutable g_in_transaction : t option ; (* Some t: t is the state before starting the transaction *)
    mutable g_ns : Rdf_iri.iri SMap.t ;
  }

type error = string
exception Error of error;;
let string_of_error s = s;;

let open_graph ?(options=[]) name =
  { g_name = name ;
    g_set_sub = Triples_s_p.empty;
    g_set_pred = Triples_p_o.empty;
    g_set_obj = Triples_o_s.empty;
    g_in_transaction = None ;
    g_ns = SMap.empty ;
  }
;;

let add_triple g ~sub ~pred ~obj =
  g.g_set_sub <- Triples_s_p.add g.g_set_sub sub pred obj ;
  g.g_set_pred <- Triples_p_o.add g.g_set_pred pred obj sub ;
  g.g_set_obj <- Triples_o_s.add g.g_set_obj obj sub pred ;
;;

let rem_triple g ~sub ~pred ~obj =
  g.g_set_sub <- Triples_s_p.rem g.g_set_sub sub pred obj ;
  g.g_set_pred <- Triples_p_o.rem g.g_set_pred pred obj sub ;
  g.g_set_obj <- Triples_o_s.rem g.g_set_obj obj sub pred ;
;;

let subjects_of g ~pred ~obj = Triples_p_o.find_list g.g_set_pred pred obj ;;

let predicates_of g ~sub ~obj = Triples_o_s.find_list g.g_set_obj obj sub ;;

let objects_of g ~sub ~pred = Triples_s_p.find_list g.g_set_sub sub pred ;;


let find ?sub ?pred ?obj g =
  match sub, pred, obj with
    None, None, None -> Triples_s_p.triples g.g_set_sub
  | Some sub, None, None -> Triples_s_p.triples_x g.g_set_sub sub []
  | None, Some pred, None ->
      List.rev_map (fun (p,o,s) -> (s, p, o)) (Triples_p_o.triples_x g.g_set_pred pred [])
  | None, None, Some obj ->
      List.rev_map (fun (o,s,p) -> (s, p, o)) (Triples_o_s.triples_x g.g_set_obj obj [])
  | Some sub, Some pred, None ->
      List.map (fun o -> (sub, pred, o)) (objects_of g ~sub ~pred)
  | Some sub, None, Some obj ->
      List.map (fun p -> (sub, p, obj)) (predicates_of g ~sub ~obj)
  | None, Some pred, Some obj ->
      List.map (fun s -> (s, pred, obj)) (subjects_of g ~pred ~obj)
  | Some sub, Some pred, Some obj ->
      let set = Triples_p_o.find g.g_set_pred pred obj in
      if Triples_p_o.Set.mem sub set then [sub, pred, obj] else []
;;

let exists ?sub ?pred ?obj g =
  match find ?sub ?pred ?obj g with [] -> false | _ -> true
;;

let subjects g = Triples_s_p.x_list g.g_set_sub;;
let predicates g = Triples_p_o.x_list g.g_set_pred;;
let objects g = Triples_o_s.x_list g.g_set_obj;;

let transaction_start g =
  let old =
    { g_name = g.g_name ;
      g_set_sub = g.g_set_sub ;
      g_set_pred = g.g_set_pred ;
      g_set_obj = g.g_set_obj ;
      g_in_transaction = g.g_in_transaction ;
      g_ns = g.g_ns ;
    }
  in
  g.g_in_transaction <- Some old
;;

let transaction_commit g =
  match g.g_in_transaction with
    None -> raise (Error "Not in a transaction.")
  | Some old -> g.g_in_transaction <- old.g_in_transaction
;;

let transaction_rollback g =
  match g.g_in_transaction with
    None -> raise (Error "Not in a transaction.")
  | Some old ->
      g.g_set_sub <- old.g_set_sub ;
      g.g_set_pred <- old.g_set_pred ;
      g.g_set_obj <- old.g_set_obj ;
      g.g_in_transaction <- old.g_in_transaction
;;

let new_blank_id g =
  let max_int = Int32.to_int (Int32.div Int32.max_int (Int32.of_int 2)) in
  let s =
    "genid"^
      (string_of_int (Triples_s_p.Map.cardinal g.g_set_sub))
      ^ "-" ^
      (string_of_int (Random.int max_int))
  in
  Rdf_term.blank_id_of_string s
;;

let graph_size g = Triples_s_p.cardinal g.g_set_sub;;

module Mem_BGP =
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

module Mem =
  struct
    let name = "mem"
    type g = t
    type error = string
    exception Error = Error
    let string_of_error = string_of_error

    let graph_name g = g.g_name
    let graph_size g = graph_size g

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

    let namespaces g =
      SMap.fold (fun name iri acc -> (iri, name) :: acc) g.g_ns []
    let add_namespace g iri name = g.g_ns <- SMap.add name iri g.g_ns
    let rem_namespace g name = g.g_ns <- SMap.remove name g.g_ns
    let set_namespaces g l =
      g.g_ns <- List.fold_left
        (fun map (iri, name) -> SMap.add name iri map) SMap.empty l

    module BGP = Mem_BGP
  end;;

Rdf_graph.add_storage (module Mem : Rdf_graph.Storage);;


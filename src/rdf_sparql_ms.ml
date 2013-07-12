(*********************************************************************************)
(*                OCaml-RDF                                                      *)
(*                                                                               *)
(*    Copyright (C) 2012-2013 Institut National de Recherche en Informatique     *)
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

open Rdf_sparql_types

module SMap = Rdf_sparql_types.SMap
module SSet = Rdf_sparql_types.SSet

(** A solution mapping : variable -> rdf term *)
type mu = Rdf_node.node SMap.t

let mu_0 = SMap.empty
let mu x t = SMap.add x t mu_0

let mu_compare = SMap.compare Rdf_node.Ord_type.compare

exception Incompatible_mus of string
exception Cannot_extend_mu of var

let mu_merge =
  let f var term1 term2 =
    match term1, term2 with
    | None, x -> x
    | x, None -> x
    | Some t1, Some t2 ->
        match Rdf_node.Ord_type.compare t1 t2 with
          0 -> Some t1
        | _ -> raise (Incompatible_mus var)
  in
  SMap.merge f

let mu_add = SMap.add
let mu_find_var v map = SMap.find v.var_name map

let mu_project =
  let f set v _ = SSet.mem v set in
  fun set mu -> SMap.filter (f set) mu

module MuOrdered =
  struct
    type t = mu
    let compare = mu_compare
  end

module MuSet = Set.Make(MuOrdered)

module MuNOrdered =
  struct
    type t = int * mu
    let compare (n1, _) (n2, _) = Pervasives.compare n1 n2
  end

module Multimu = Set.Make(MuNOrdered)

(** A Multiset is a set of pairs (int, mu) *)
type multiset = Multimu.t

let omega_add =
  let genid =
    let cpt = ref 0 in
    fun () -> incr cpt; !cpt
  in
  fun mu ms ->
    Multimu.add (genid(), mu) ms
;;

let omega_0 = omega_add mu_0 Multimu.empty
let omega x t = omega_add (mu x t) Multimu.empty

let card_mu omega mu0 =
  let pred (_,mu) = mu_compare mu0 mu = 0 in
  let s = Multimu.filter pred omega in
  Multimu.cardinal s
;;

let omega_filter =
  let f pred (id, mu) set =
    if pred mu then Multimu.add (id, mu) set else set
  in
  fun pred om ->
    Multimu.fold (f pred) om Multimu.empty
;;

let omega_join =
  let f2 pred mu1 (_, mu2) set =
    try
      let mu = mu_merge mu1 mu2 in
      if pred mu then
        omega_add mu set
      else
        set
    with Incompatible_mus _ -> set
  in
  let f pred om2 (_id1, mu1) set =
    Multimu.fold (f2 pred mu1) om2 set
  in
  fun ?(pred=fun _ -> true) om1 om2 -> Multimu.fold (f pred om2) om1 Multimu.empty
  ;;

let omega_union = Multimu.union;;

let omega_diff_pred =
  let pred eval mu1 (_, mu2) =
    try
      let mu = mu_merge mu1 mu2 in
      not (eval mu)
      with Incompatible_mus _ -> true
  in
  let f eval o2 (_, mu1) =
     Multimu.for_all (pred eval mu1) o2
  in
  fun eval o1 o2 ->
    match Multimu.compare o2 omega_0 with
      0 -> o1
    | _ ->
      match Multimu.compare o2 Multimu.empty with
        0 -> o1
      | _ -> Multimu.filter (f eval o2) o1
;;

exception Not_disjoint
let mu_disjoint_doms =
  let f _ v1 v2 =
    match v1, v2 with
      Some _, Some _ -> raise Not_disjoint
    | _ -> None
  in
  fun mu1 mu2 ->
    try ignore(SMap.merge f mu1 mu2); true
    with Not_disjoint -> false
;;

let omega_minus =
  let f2 mu1 (_, mu2) =
    (mu_disjoint_doms mu1 mu2) ||
      (try ignore (mu_merge mu1 mu2); false
       with _ -> true)
  in
  let f o2 (_, mu1) = Multimu.for_all (f2 mu1) o2 in
  fun o1 o2 -> Multimu.filter (f o2) o1

let omega_extend =
  let f eval var (_, mu) map =
    let mu =
      try
        ignore(SMap.find var.var_name mu);
        raise (Cannot_extend_mu var)
      with
        Not_found ->
          try
            let v = eval mu in
            mu_add var.var_name v mu
          with
            _ -> mu
    in
    omega_add mu map
  in
  fun eval o var ->
    Multimu.fold (f eval var) o Multimu.empty
;;

let omega_fold =
  let f g (_, mu) acc = g mu acc in
  fun g o acc -> Multimu.fold (f g) o acc
;;

let omega_iter =
  let f g (_, mu) = g mu in
  fun g o -> Multimu.iter (f g) o
;;

  
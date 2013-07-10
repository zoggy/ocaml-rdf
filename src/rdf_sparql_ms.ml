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

let mu_project =
  let f set v _ = SSet.mem v set in
  fun set mu -> SMap.filter (f set) mu

module MuOrdered =
  struct
    type t = mu
     let compare = mu_compare
  end

module MuMap = Map.Make(MuOrdered)
module MuSet = Set.Make(MuOrdered)

(** A Multiset is a map from a mu to a counter *)
type multiset = int MuMap.t

let omega_add mu ms =
  try
    let n = MuMap.find mu ms in
    MuMap.add mu (n+1) ms
  with Not_found ->
    MuMap.add mu 1 ms
;;

let omega_0 = MuMap.add mu_0 1 MuMap.empty
let omega x t = MuMap.add (mu x t) 1 MuMap.empty

let card_mu omega mu =
  try MuMap.find mu omega
  with Not_found -> 0
;;

let omega_filter =
  let f pred mu n map =
    if pred mu then MuMap.add mu n map else map
  in
  fun pred om ->
    MuMap.fold (f pred) om MuMap.empty
;;

let omega_join =
  let f2 pred mu1 n1 mu2 n2 map =
    try
      let mu = mu_merge mu1 mu2 in
      if pred mu then
        MuMap.add mu (n1 * n2) map
      else
        map
    with Incompatible_mus _ -> map
  in
  let f pred om2 mu1 n1 map =
    MuMap.fold (f2 pred mu1 n1) om2 map
  in
  fun ?(pred=fun _ -> true) om1 om2 -> MuMap.fold (f pred om2) om1 MuMap.empty
  ;;

let omega_union =
  let f key n1 n2 =
    match n1, n2 with
      None, x
    | x, None -> x
    | Some n1, Some n2 -> Some (n1 + n2)
  in
  MuMap.merge f
;;

let omega_diff_pred =
  let pred eval mu1 mu2 _ =
    try
      let mu = mu_merge mu1 mu2 in
      not (eval mu)
      with Incompatible_mus _ -> true
  in
  let f eval o2 mu1 _ =
     MuMap.for_all (pred eval mu1) o2
  in
  fun eval o1 o2 ->
    match MuMap.compare Pervasives.compare o2 omega_0 with
      0 -> o1
    | _ ->
      match MuMap.compare Pervasives.compare o2 MuMap.empty with
        0 -> o1
      | _ -> MuMap.filter (f eval o2) o1
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
  let f2 mu1 mu2 _ =
    (mu_disjoint_doms mu1 mu2) ||
      (try ignore (mu_merge mu1 mu2); false
       with _ -> true)
  in
  let f o2 mu1 _ = MuMap.for_all (f2 mu1) o2 in
  fun o1 o2 -> MuMap.filter (f o2) o1

let omega_extend =
  let f eval var mu n map =
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
    MuMap.add mu n map
  in
  fun eval o var ->
    MuMap.fold (f eval var) o MuMap.empty
;;

let omega_fold =
  let rec iteri n f mu acc i =
    if i < n then
      iteri n f mu (f mu acc) (i+1)
    else
      acc
  in
  let on_mu f mu n acc = iteri n f mu acc 0 in
  fun f o acc -> MuMap.fold (on_mu f) o acc
;;

let omega_fold_n = MuMap.fold;;

let omega_iter =
  let rec iteri n f mu i =
    if i < n then
     ( f mu; iteri n f mu (i+1) )
  in
  let on_mu f mu n = iteri n f mu 0 in
  fun f o -> MuMap.iter (on_mu f) o
;;

  
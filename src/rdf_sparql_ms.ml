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

(** *)

open Rdf_sparql_types

let dbg = Rdf_misc.create_log_fun
  ~prefix: "Rdf_sparql_ms"
    "RDF_SPARQL_MS_DEBUG_LEVEL"
;;
module SMap = Rdf_sparql_types.SMap
module SSet = Rdf_sparql_types.SSet

module VMap = Rdf_dt.VMap;;


exception Incompatible_mus of string
exception Cannot_extend_mu of var

(** A solution mapping : variable -> rdf term *)
type mu = {
  mu_bindings : Rdf_term.term SMap.t ;
  mutable mu_bnodes : string VMap.t ;
}

let mu_0 = { mu_bindings = SMap.empty ; mu_bnodes = VMap.empty }
let mu_add v t mu = { mu with mu_bindings = SMap.add v t mu.mu_bindings }
let mu_copy mu = { mu_bindings = mu.mu_bindings ; mu_bnodes = mu.mu_bnodes }
let mu x t = mu_add x t mu_0

let gen_blank_id =
  let cpt = ref 0 in
  fun () -> incr cpt ; "__b"^(string_of_int !cpt)^"__"
;;

let get_bnode mu value =
  try Rdf_dt.Blank (VMap.find value mu.mu_bnodes)
  with Not_found ->
    let label = gen_blank_id () in
    mu.mu_bnodes <- VMap.add value label mu.mu_bnodes ;
    Rdf_dt.Blank label
;;

let mu_compare mu1 mu2 =
  SMap.compare Rdf_term.compare mu1.mu_bindings mu2.mu_bindings
;;

let mu_merge =
  let f var term1 term2 =
    match term1, term2 with
    | None, x -> x
    | x, None -> x
    | Some t1, Some t2 ->
        match Rdf_term.compare t1 t2 with
          0 -> Some t1
        | _ -> raise (Incompatible_mus var)
  in
  let merge_bnodes v label1 label2 =
    match label1, label2 with
      None, x | x, None -> x
    | Some l1, Some l2 -> Some l1
        (*match Pervasives.compare l1 l2 with
          0 -> Some l1
        | _ ->
          (*dbg ~loc: "warning" ~level:2 (fun () -> "Merging mus: bnodes label maps differ");*)
          Some l1*)
  in
  fun mu1 mu2 ->
    let mu_bindings = SMap.merge f mu1.mu_bindings mu2.mu_bindings in
    let mu_bnodes = VMap.merge merge_bnodes mu1.mu_bnodes mu2.mu_bnodes in
    { mu_bindings ; mu_bnodes }

let mu_find_varname name mu = SMap.find name mu.mu_bindings
let mu_find_var v mu = SMap.find v.var_name mu.mu_bindings

let mu_project =
  let f set v _ = SSet.mem v set in
  fun set mu -> { mu with mu_bindings = SMap.filter (f set) mu.mu_bindings }

let mu_fold f mu acc = SMap.fold f mu.mu_bindings acc;;
let mu_iter f mu = SMap.iter f mu.mu_bindings;;

module MuOrdered =
  struct
    type t = mu
    let compare = mu_compare
  end

module MuSet = Set.Make(MuOrdered)

module MuNOrdered =
  struct
    type t = int * mu
    let compare (n1, _) (n2, _) = n1 - n2
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

let omega_add_if_not_present =
  let pred mu0 (_,mu) = mu_compare mu0 mu = 0 in
  fun mu ms ->
    if Multimu.exists (pred mu) ms
    then ms
    else omega_add mu ms
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
    try ignore(SMap.merge f mu1.mu_bindings mu2.mu_bindings); true
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
        ignore(mu_find_var var mu);
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

let omega_exists =
  let f pred (_, mu) = pred mu in
  fun pred o -> Multimu.exists (f pred) o
;;

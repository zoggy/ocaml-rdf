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

val dbg : ?loc:string -> ?level:int -> (unit -> string) -> unit
module SMap = Rdf_sparql_types.SMap
module SSet = Rdf_sparql_types.SSet
module VMap = Rdf_dt.VMap
exception Incompatible_mus of string
exception Cannot_extend_mu of Rdf_sparql_types.var
type mu = {
  mu_bindings : Rdf_term.term SMap.t;
  mutable mu_bnodes : string VMap.t;
}
val mu_0 : mu
val mu_add : SMap.key -> Rdf_term.term -> mu -> mu
val mu_copy : mu -> mu
val mu : SMap.key -> Rdf_term.term -> mu
val gen_blank_id : unit -> string
val get_bnode : mu -> VMap.key -> Rdf_dt.value
val mu_compare : mu -> mu -> int
val mu_merge : mu -> mu -> mu
val mu_find_varname : SMap.key -> mu -> Rdf_term.term
val mu_find_var : Rdf_sparql_types.var -> mu -> Rdf_term.term
val mu_project : SSet.t -> mu -> mu
val mu_fold : (SMap.key -> Rdf_term.term -> 'a -> 'a) -> mu -> 'a -> 'a
val mu_iter : (SMap.key -> Rdf_term.term -> unit) -> mu -> unit
module MuOrdered : sig type t = mu val compare : mu -> mu -> int end
module MuSet :
  sig
    type elt = MuOrdered.t
    type t = Set.Make(MuOrdered).t
    val empty : t
    val is_empty : t -> bool
    val mem : elt -> t -> bool
    val add : elt -> t -> t
    val singleton : elt -> t
    val remove : elt -> t -> t
    val union : t -> t -> t
    val inter : t -> t -> t
    val diff : t -> t -> t
    val compare : t -> t -> int
    val equal : t -> t -> bool
    val subset : t -> t -> bool
    val iter : (elt -> unit) -> t -> unit
    val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
    val for_all : (elt -> bool) -> t -> bool
    val exists : (elt -> bool) -> t -> bool
    val filter : (elt -> bool) -> t -> t
    val partition : (elt -> bool) -> t -> t * t
    val cardinal : t -> int
    val elements : t -> elt list
    val min_elt : t -> elt
    val max_elt : t -> elt
    val choose : t -> elt
    val split : elt -> t -> t * bool * t
    val find : elt -> t -> elt
    val of_list : elt list -> t
  end
module MuNOrdered :
  sig type t = int * mu val compare : int * 'a -> int * 'b -> int end
module Multimu :
  sig
    type elt = MuNOrdered.t
    type t = Set.Make(MuNOrdered).t
    val empty : t
    val is_empty : t -> bool
    val mem : elt -> t -> bool
    val add : elt -> t -> t
    val singleton : elt -> t
    val remove : elt -> t -> t
    val union : t -> t -> t
    val inter : t -> t -> t
    val diff : t -> t -> t
    val compare : t -> t -> int
    val equal : t -> t -> bool
    val subset : t -> t -> bool
    val iter : (elt -> unit) -> t -> unit
    val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
    val for_all : (elt -> bool) -> t -> bool
    val exists : (elt -> bool) -> t -> bool
    val filter : (elt -> bool) -> t -> t
    val partition : (elt -> bool) -> t -> t * t
    val cardinal : t -> int
    val elements : t -> elt list
    val min_elt : t -> elt
    val max_elt : t -> elt
    val choose : t -> elt
    val split : elt -> t -> t * bool * t
    val find : elt -> t -> elt
    val of_list : elt list -> t
  end
type multiset = Multimu.t
val omega_add : mu -> Multimu.t -> Multimu.t
val omega_add_if_not_present : mu -> Multimu.t -> Multimu.t
val omega_0 : Multimu.t
val omega : SMap.key -> Rdf_term.term -> Multimu.t
val card_mu : Multimu.t -> mu -> int
val omega_filter : (mu -> bool) -> Multimu.t -> Multimu.t
val omega_join : ?pred:(mu -> bool) -> Multimu.t -> Multimu.t -> Multimu.t
val omega_union : Multimu.t -> Multimu.t -> Multimu.t
val omega_diff_pred : (mu -> bool) -> Multimu.t -> Multimu.t -> Multimu.t
exception Not_disjoint
val mu_disjoint_doms : mu -> mu -> bool
val omega_minus : Multimu.t -> Multimu.t -> Multimu.t
val omega_extend :
  (mu -> Rdf_term.term) -> Multimu.t -> Rdf_sparql_types.var -> Multimu.t
val omega_fold : (mu -> 'a -> 'a) -> Multimu.t -> 'a -> 'a
val omega_iter : (mu -> unit) -> Multimu.t -> unit
val omega_exists : (mu -> bool) -> Multimu.t -> bool

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

(** Evaluation of Basic Graph Patterns (BGP). *)

module type P =
  sig
    type g
    type term
    val term : Rdf_term.term -> term
    val compare : term -> term -> int
    val rdfterm : term -> Rdf_term.term
    val subjects : unit -> term list
    val objects : unit -> term list
    val find :
      ?sub:term ->
      ?pred:term->
      ?obj:term -> unit -> (term * term * term) list
  end

module type S =
  sig
    val eval_bgp :
      Rdf_sparql_algebra.triple list -> Rdf_sparql_ms.Multimu.t
  end

module Make : functor (P : P) -> S

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

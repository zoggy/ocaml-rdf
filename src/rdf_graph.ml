(** *)

open Rdf_types;;



module type Storage =
  sig
    type g

    val open_graph : options: (string * string) list -> string -> g

    val add_triple : g -> sub: node -> pred: node -> obj: node -> unit
    val rem_triple : g -> sub: node -> pred: node -> obj: node -> unit

    val add_triple_t : g -> triple -> unit
    val rem_triple_t : g -> triple -> unit

    val subjects : g -> pred: node -> obj: node -> node list
    val predicates : g -> sub: node -> obj: node -> node list
    val objects : g -> sub: node -> pred: node -> node list

    val find : g -> ?sub: node -> ?pred: node -> ?obj: node -> triple list
    val exists : g -> ?sub: node -> ?pred: node -> ?obj: node -> bool
    val exists_t : g -> triple -> bool
  end

module Make (S : Storage) =
  struct


  end
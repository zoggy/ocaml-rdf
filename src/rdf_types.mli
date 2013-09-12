(** Base types. *)

module Ordered_string : sig
    type t = string
    val compare : t -> t -> int
  end
module SMap : Map.S with type key = string
module SSet : Set.S with type elt = string

module Ordered_char : sig
    type t = char
    val compare : t -> t -> int
  end
module CMap : Map.S with type key = char
module CSet : Set.S with type elt = char
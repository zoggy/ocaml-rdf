(** *)

module Ordered_string = struct type t = string let compare = String.compare end;;
module SMap = Map.Make (Ordered_string);;
module SSet = Set.Make (Ordered_string);;

module Ordered_char = struct type t = char let compare = Char.compare end;;
module CMap = Map.Make (Ordered_char);;
module CSet = Set.Make (Ordered_char);;
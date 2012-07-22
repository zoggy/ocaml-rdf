(** Memory storage, using tree-based sets. *)

let dbg = Rdf_misc.create_log_fun
  ~prefix: "Rdf_mem"
    "RDF_MEM_DEBUG_LEVEL"
;;

type id = string

module Id = struct type t = id let compare = Pervasives.compare end

module Triples =
  struct
    module Set = Set.Make(Id)
    module Map = Map.Make(Id)
    type t = Set.t Map.t Map.t
  end
;;

module SMap = Map.Make
  (struct type t = string let compare = Pervasives.compare end)
;;

type t =
  { g_name : Rdf_uri.uri ; (* graph name *)
    g_nodes : Rdf_node.node SMap.t  ; (* associate ids to nodes *)
    g_set_sub : Triples.t ; (* sub -> (pred -> obj set) *)
    g_set_pred : Triples.t ; (* pred -> (obj -> sub set) *)
    g_set_obj : Triples.t ; (* obj -> (pred -> sub set) *)
    mutable g_in_transaction : bool ;
  }

type error = string
exception Error of error;;
let string_of_error s = s;;

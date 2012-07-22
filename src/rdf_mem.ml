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
    let empty = Map.empty

    let add t x y z =
      let m =
        try Map.find x t
        with Not_found -> Map.empty
      in
      let set =
        try Set.add z (Map.find y m)
        with Not_found -> Set.singleton z
      in
      let m = Map.add y set m in
      Map.add x m t
  end
;;

module SMap = Map.Make
  (struct type t = string let compare = Pervasives.compare end)
;;

type t =
  { g_name : Rdf_uri.uri ; (* graph name *)
    mutable g_nodes : Rdf_node.node SMap.t  ; (* associate ids to nodes *)
    mutable g_set_sub : Triples.t ; (* sub -> (pred -> obj set) *)
    mutable _set_pred : Triples.t ; (* pred -> (obj -> sub set) *)
    mutable g_set_obj : Triples.t ; (* obj -> (pred -> sub set) *)
    mutable g_in_transaction : t option ; (* Some t: t is the state before starting the transaction *)
  }

type error = string
exception Error of error;;
let string_of_error s = s;;

let open_graph ?(options=[]) name =
  { g_name = name ;
    g_nodes = SMap.empty ;
    g_set_sub = Triples.empty;
    g_set_pred = Triples.empty;
    g_set_obj = Triples.empty;
    g_in_transaction = None ;
  }
;;

let add_triple g ~sub ~pred ~obj = ()

;;

module Mem =
  struct
    let name = "mem"
    type g = t
    type error = string
    exception Error = Error
    let string_of_error = string_of_error

    let graph_name g = g.g_name

    let open_graph = open_graph

    let add_triple = add_triple
    let rem_triple = rem_triple

    let add_triple_t g (sub, pred, obj) = add_triple g ~sub ~pred ~obj
    let rem_triple_t g (sub, pred, obj) = rem_triple g ~sub ~pred ~obj

    let subjects_of = subjects_of
    let predicates_of = predicates_of
    let objects_of = objects_of

    let find = find
    let exists = exists
    let exists_t (sub, pred, obj) g = exists ~sub ~pred ~obj g

    let subjects = subjects
    let predicates = predicates
    let objects = objects

    let transaction_start = transaction_start
    let transaction_commit = transaction_commit
    let transaction_rollback = transaction_rollback
  end;;

Rdf_graph.add_storage (module Mem : Rdf_graph.Storage);;


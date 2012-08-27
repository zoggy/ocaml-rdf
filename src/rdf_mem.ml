(** Memory storage, using tree-based sets. *)

let dbg = Rdf_misc.create_log_fun
  ~prefix: "Rdf_mem"
    "RDF_MEM_DEBUG_LEVEL"
;;

module Id = struct type t = Rdf_node.node let compare = Pervasives.compare end

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

    let rem t x y z =
      let m =
        try Map.find x t
        with Not_found -> Map.empty
      in
      try
        let set = Set.remove z (Map.find y m) in
        let m = Map.add y set m in
        Map.add x m t
      with
        Not_found -> t

    let find t x y =
      try Map.find y (Map.find x t)
      with Not_found -> Set.empty

    let find_list t x y =
      Set.fold (fun elt acc -> elt :: acc) (find t x y) []

    let find2_list t x z =
      let f y set acc =
        if Set.mem z set then y :: acc else acc
      in
      try
        let m = Map.find x t in
        Map.fold f m []
      with Not_found -> []


    let triples_y x y set acc =
      let fz x y z acc = (x,y,z) :: acc in
      Set.fold (fz x y) set acc

    let triples_x t x acc =
      try Map.fold (triples_y x) (Map.find x t) acc
      with Not_found -> acc

    let triples =
      let fx elt map acc =
        Map.fold (triples_y elt) map acc
      in
      fun t -> Map.fold fx t []

    let x_list =
      let pred _ set = not (Set.is_empty set) in
      let fx elt map acc =
        if Map.exists pred map then elt :: acc else acc
      in
      fun t -> Map.fold fx t []
  end
;;

type t =
  { g_name : Rdf_uri.uri ; (* graph name *)
    mutable g_set_sub : Triples.t ; (* sub -> (pred -> obj set) *)
    mutable g_set_pred : Triples.t ; (* pred -> (obj -> sub set) *)
    mutable g_set_obj : Triples.t ; (* obj -> (pred -> sub set) *)
    mutable g_in_transaction : t option ; (* Some t: t is the state before starting the transaction *)
  }

type error = string
exception Error of error;;
let string_of_error s = s;;

let open_graph ?(options=[]) name =
  { g_name = name ;
    g_set_sub = Triples.empty;
    g_set_pred = Triples.empty;
    g_set_obj = Triples.empty;
    g_in_transaction = None ;
  }
;;

let add_triple g ~sub ~pred ~obj =
  g.g_set_sub <- Triples.add g.g_set_sub sub pred obj ;
  g.g_set_pred <- Triples.add g.g_set_pred pred obj sub ;
  g.g_set_obj <- Triples.add g.g_set_obj obj pred sub
;;

let rem_triple g ~sub ~pred ~obj =
  g.g_set_sub <- Triples.rem g.g_set_sub sub pred obj ;
  g.g_set_pred <- Triples.rem g.g_set_pred pred obj sub ;
  g.g_set_obj <- Triples.rem g.g_set_obj obj pred sub
;;

let subjects_of g ~pred ~obj = Triples.find_list g.g_set_pred pred obj ;;

let predicates_of g ~sub ~obj = Triples.find2_list g.g_set_pred sub obj ;;

let objects_of g ~sub ~pred = Triples.find_list g.g_set_sub sub pred ;;


let find ?sub ?pred ?obj g =
  match sub, pred, obj with
    None, None, None -> Triples.triples g.g_set_sub
  | Some sub, None, None -> Triples.triples_x g.g_set_sub sub []
  | None, Some pred, None ->
      List.rev_map (fun (p,o,s) -> (s, p, o)) (Triples.triples_x g.g_set_pred pred [])
  | None, None, Some obj ->
      List.rev_map (fun (o,p,s) -> (s, p, o)) (Triples.triples_x g.g_set_obj obj [])
  | Some sub, Some pred, None ->
      List.map (fun o -> (sub, pred, o)) (objects_of g ~sub ~pred)
  | Some sub, None, Some obj ->
      List.map (fun p -> (sub, p, obj)) (predicates_of g ~sub ~obj)
  | None, Some pred, Some obj ->
      List.map (fun s -> (s, pred, obj)) (subjects_of g ~pred ~obj)
  | Some sub, Some pred, Some obj ->
      let set = Triples.find g.g_set_pred pred obj in
      if Triples.Set.mem sub set then [sub, pred, obj] else []
;;

let exists ?sub ?pred ?obj g =
  match find ?sub ?pred ?obj g with [] -> false | _ -> true
;;

let subjects g = Triples.x_list g.g_set_sub;;
let predicates g = Triples.x_list g.g_set_pred;;
let objects g = Triples.x_list g.g_set_obj;;

let transaction_start g =
  let old =
    { g_name = g.g_name ;
      g_set_sub = g.g_set_sub ;
      g_set_pred = g.g_set_pred ;
      g_set_obj = g.g_set_obj ;
      g_in_transaction = g.g_in_transaction ;
    }
  in
  g.g_in_transaction <- Some old
;;

let transaction_commit g =
  match g.g_in_transaction with
    None -> raise (Error "Not in a transaction.")
  | Some old -> g.g_in_transaction <- old.g_in_transaction
;;

let transaction_rollback g =
  match g.g_in_transaction with
    None -> raise (Error "Not in a transaction.")
  | Some old ->
      g.g_set_sub <- old.g_set_sub ;
      g.g_set_pred <- old.g_set_pred ;
      g.g_set_obj <- old.g_set_obj ;
      g.g_in_transaction <- old.g_in_transaction
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


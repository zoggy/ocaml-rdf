(** *)

open Rdf_uri;;
open Rdf_node;;

type options = (string * string) list
let get_option ?def name l =
  try List.assoc name l
  with Not_found ->
    match def with
        None -> failwith (Printf.sprintf "Missing option %S" name)
      | Some v -> v
;;

module type Storage =
  sig
    val name : string

    type g

    type error
    exception Error of error
    val string_of_error : error -> string

    val open_graph : ?options: (string * string) list -> uri -> g
    val graph_name : g -> uri

    val add_triple : g -> sub: node -> pred: node -> obj: node -> unit
    val rem_triple : g -> sub: node -> pred: node -> obj: node -> unit

    val add_triple_t : g -> triple -> unit
    val rem_triple_t : g -> triple -> unit

    val subjects_of : g -> pred: node -> obj: node -> node list
    val predicates_of : g -> sub: node -> obj: node -> node list
    val objects_of : g -> sub: node -> pred: node -> node list

    val find : ?sub: node -> ?pred: node -> ?obj: node -> g -> triple list
    val exists : ?sub: node -> ?pred: node -> ?obj: node -> g -> bool
    val exists_t : triple -> g -> bool

    val subjects : g -> node list
    val predicates : g -> node list
    val objects : g -> node list

    val transaction_start : g -> unit
    val transaction_commit : g -> unit
    val transaction_rollback : g -> unit

    val new_blank_id : g -> Rdf_node.blank_id
  end

exception Storage_error of string * string * exn
module Make (S : Storage) =
  struct
    type g = S.g

    let embed f x =
      try f x
      with (S.Error e) as exn ->
        raise (Storage_error (S.name, S.string_of_error e, exn))

    let open_graph ?options name = embed (S.open_graph ?options) name
    let graph_name = embed S.graph_name

    let add_triple g ~sub ~pred ~obj = embed (fun g -> S.add_triple g ~sub ~pred ~obj) g
    let rem_triple g ~sub ~pred ~obj = embed (fun g -> S.rem_triple g ~sub ~pred ~obj) g

    let add_triple_t g = embed (S.add_triple_t g)
    let rem_triple_t g = embed (S.rem_triple_t g)

    let subjects_of g ~pred ~obj = embed (fun g -> S.subjects_of g ~pred ~obj) g
    let predicates_of g ~sub ~obj = embed (fun g -> S.predicates_of g ~sub ~obj) g
    let objects_of g ~sub ~pred = embed (fun g -> S.objects_of g ~sub ~pred) g

    let find ?sub ?pred ?obj = embed (S.find ?sub ?pred ?obj)
    let exists ?sub ?pred ?obj = embed (S.exists ?sub ?pred ?obj)
    let exists_t triple = embed (S.exists_t triple)

    let subjects = embed S.subjects
    let predicates = embed S.predicates
    let objects = embed S.objects

    let transaction_start = embed S.transaction_start
    let transaction_commit = embed S.transaction_commit
    let transaction_rollback = embed S.transaction_rollback

    let new_blank_id = embed S.new_blank_id
  end

module type Graph =
  sig
    type g

    val open_graph : ?options: (string * string) list -> uri -> g
    val graph_name : g -> uri

    val add_triple : g -> sub: node -> pred: node -> obj: node -> unit
    val rem_triple : g -> sub: node -> pred: node -> obj: node -> unit

    val add_triple_t : g -> triple -> unit
    val rem_triple_t : g -> triple -> unit

    val subjects_of : g -> pred: node -> obj: node -> node list
    val predicates_of : g -> sub: node -> obj: node -> node list
    val objects_of : g -> sub: node -> pred: node -> node list

    val find : ?sub: node -> ?pred: node -> ?obj: node -> g -> triple list
    val exists : ?sub: node -> ?pred: node -> ?obj: node -> g -> bool
    val exists_t : triple -> g -> bool

    val subjects : g -> node list
    val predicates : g -> node list
    val objects : g -> node list

    val transaction_start : g -> unit
    val transaction_commit : g -> unit
    val transaction_rollback : g -> unit

    val new_blank_id : g -> Rdf_node.blank_id
  end

let storages = ref [];;
let add_storage m =
  let module P = (val m : Storage) in
  let module M = Make (P) in
  storages := (P.name, (module M : Graph)) :: !storages
;;

type graph =
  {
    name : unit -> uri ;
    add_triple : sub: node -> pred: node -> obj: node -> unit ;
    rem_triple : sub: node -> pred: node -> obj: node -> unit ;
    add_triple_t : (node * node * node) -> unit ;
    rem_triple_t : (node * node * node) -> unit ;
    subjects_of : pred: node -> obj: node -> node list ;
    predicates_of : sub: node -> obj: node -> node list ;
    objects_of : sub: node -> pred: node -> node list ;
    find : ?sub: node -> ?pred: node -> ?obj: node -> unit -> triple list ;
    exists : ?sub: node -> ?pred: node -> ?obj: node -> unit -> bool ;
    exists_t : triple -> bool ;
    subjects : unit -> node list ;
    predicates : unit -> node list ;
    objects : unit -> node list ;
    transaction_start : unit -> unit ;
    transaction_commit : unit -> unit ;
    transaction_rollback : unit -> unit ;
    new_blank_id : unit -> Rdf_node.blank_id ;
    namespaces : unit -> (uri * string) list ;
  }


let open_graph ?(options=[]) name =
  let kind = get_option ~def: "memory" "storage" options in
  let storage =
    try List.assoc kind !storages
    with Not_found -> failwith (Printf.sprintf "Unknown storage %S" kind)
  in
  let module S = (val storage) in
  let g = S.open_graph ~options name in
  let namespaces () =
    let triples = S.find ~pred: (Uri Rdf_rdf.ordf_ns) g in
    let f  (sub, _ , obj) =
      let uri = match sub with
        | Uri uri -> uri
        | _ -> failwith "Invalid namespace uri"
      in
      let prefix = match obj with
        | Literal lit -> lit.lit_value
        | _ -> failwith "Invalid namespace prefix"
      in
      (uri, prefix)
    in
    List.map f triples
  in
  { name = (fun () -> S.graph_name g) ;
    add_triple = S.add_triple g ;
    rem_triple = S.rem_triple g ;
    add_triple_t = S.add_triple_t g ;
    rem_triple_t = S.rem_triple_t g ;
    subjects_of = S.subjects_of g ;
    predicates_of = S.predicates_of g ;
    objects_of = S.objects_of g ;
    find = (fun ?sub ?pred ?obj () -> S.find ?sub ?pred ?obj g) ;
    exists = (fun ?sub ?pred ?obj () -> S.exists ?sub ?pred ?obj g) ;
    exists_t = (fun t -> S.exists_t t g) ;
    subjects = (fun () -> S.subjects g) ;
    predicates = (fun () -> S.predicates g) ;
    objects = (fun () -> S.objects g) ;
    transaction_start = (fun () -> S.transaction_start g) ;
    transaction_commit = (fun () -> S.transaction_commit g) ;
    transaction_rollback = (fun () -> S.transaction_rollback g) ;
    new_blank_id = (fun () -> S.new_blank_id g) ;
    namespaces = namespaces ;
  }
;;
(** Reading and writing RDF/XML. *)

open Rdf_graph;;
open Rdf_node;;

(** {2 Using trees for XML docs}
  Code taken from Xmlm examples.
  Thanks to Xmlm, namespaces are already handled by the parser :-)
*)

type tree = E of Xmlm.tag * tree list | D of string

let in_tree i =
  let el tag childs = E (tag, childs)  in
  let data d = D d in
  Xmlm.input_doc_tree ~el ~data i

let out_tree o t =
  let frag = function
  | E (tag, childs) -> `El (tag, childs)
  | D d -> `Data d
  in
  Xmlm.output_doc_tree frag o t

(** {2 Constants defined in
  {{:http://www.w3.org/TR/rdf-syntax-grammar/}RDF grammar}. } *)

let rdf_ s = Rdf_uri.uri ("http://www.w3.org/1999/02/22-rdf-syntax-ns#"^s);;

(** {3 Syntax names} *)
let rdf_about = rdf_"about";;
let rdf_datatype = rdf_"datatype";;
let rdf_Descriptin = rdf_"Description";;
let rdf_ID = rdf_"ID";;
let rdf_li = rdf_"li";;
let rdf_nodeID = rdf_"nodeID";;
let rdf_RDF = rdf_"RDF";;
let rdf_parseType = rdf_"parseType";;
let rdf_resource = rdf_"resource";;

(** {3 Class names} *)
let rdf_Alt = rdf_"Alt";;
let rdf_Bag = rdf_"Bag";;
let rdf_List = rdf_"List";;
let rdf_Property = rdf_"Property";;
let rdf_Seq = rdf_"Seq";;
let rdf_Statement = rdf_"Statement";;
let rdf_XMLLiteral = rdf_"XMLLiteral";;

(** {3 Property names} *)

let rdf_subject = rdf_"subject";;
let rdf_predicate = rdf_"predicate";;
let rdf_object = rdf_"object";;
let rdf_type = rdf_"type";;
let rdf_value = rdf_"value";;
let rdf_first = rdf_"first";;
let rdf_rest = rdf_"rest";;
let rdf_n n = rdf_("_"^(string_of_int n));;

let is_element uri (pref,loc) =
  let uri2 = Rdf_uri.uri (pref^loc) in
  Rdf_uri.compare uri uri2 = 0
;;

(** {3 Resource names} *)
let rdf_nil = rdf_"nil"

(** {2 Input} *)

type state =
  { subject : Rdf_uri.uri option ;
    predicate : Rdf_uri.uri option ;
    xml_base : Rdf_uri.uri ;
    xml_lang : string option ;
    datatype : Rdf_uri.uri option ;
  }

module SMap = Map.Make (struct type t = string let compare = Pervasives.compare end);;
type global_state =
  {
    blanks : Rdf_node.blank_id SMap.t ;
  }

exception Invalid_rdf of string
let error s = raise (Invalid_rdf s);;

let get_att att l = try Some (List.assoc att l) with Not_found -> None;;
let get_att_uri =
  let rec iter pred = function
    [] -> None
  | (x,v) :: q ->
    if pred x then Some v else iter pred q
  in
  fun uri l -> iter (is_element uri) l
;;

let set_xml_base state = function
  D _ -> state
| E ((_,atts),_) ->
    match get_att ("xml", "base") atts with
      None -> state
    | Some s -> { state with xml_base = Rdf_uri.uri s }
;;
let set_xml_lang state = function
  D _ -> state
| E ((_,atts),_) ->
    match get_att ("xml", "lang") atts with
      None -> state
    | Some s -> { state with xml_lang = Some s }
;;

let update_state state t = set_xml_lang (set_xml_base state t) t;;
let get_blank_node g gstate id =
  try (Blank_ (SMap.find id gstate.blanks), gstate)
  with Not_found ->
    let bid = g.new_blank_id () in
    let gstate = { gstate with blanks = SMap.add id bid gstate.blanks } in
    (Blank_ bid, gstate)


let rec input_node g state gstate t =
  let state = update_state state t in
  match t with
    D s when state.predicate = None ->
      let msg = Printf.sprintf "Found Data (%S) with no current predicate." s in
      error msg
  | D s ->
      let obj = Rdf_node.node_of_literal_string ?typ: state.datatype ?lang: state.xml_lang s in
      let sub = match state.subject with None -> assert false | Some s -> Uri s in
      let pred = match state.subject with None -> assert false | Some u -> Uri u in
      g.add_triple ~sub ~pred ~obj;
      gstate
  | E (((pref,s), atts), children) ->
      let (node, gstate) =
        match get_att_uri rdf_about atts with
          Some s -> (Uri (Rdf_uri.uri s), gstate)
        | None ->
            match get_att_uri rdf_ID atts with
              Some id -> (Uri (Rdf_uri.concat state.xml_base ("#"^id)), gstate)
            | None ->
                match get_att_uri rdf_nodeID atts with
                  Some id -> get_blank_node g gstate id
                | None -> (Blank, gstate)
      in
      assert false

and input_prop g state gstate t = gstate

let input g ~base t =
  let state = {
      subject = None ; predicate = None ;
      xml_base = base ; xml_lang = None ;
      datatype = None ;
    }
  in
  let state = update_state state t in
  let gstate = { blanks = SMap.empty } in
  let gstate =
    match t with
      D _ -> assert false
    | E ((e,_),children) when is_element rdf_RDF e ->
        List.fold_left (input_node g state) gstate children
    | t -> input_node g state gstate t
  in
  ignore(gstate)
;;

let input_file g ~base file =
  let ic = open_in file in
  let i = Xmlm.make_input (`Channel ic) in
  let (_,tree) =
    try let t = in_tree i in close_in ic; t
    with e -> close_in ic; raise e
  in
  input g ~base tree
;;

(** {2 Output} *)

let output g = failwith "Not implemented";;
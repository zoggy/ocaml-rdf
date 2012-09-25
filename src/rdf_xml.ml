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

let string_of_xmls trees =
  try
    let b = Buffer.create 256 in
    let output = Xmlm.make_output ~decl: false (`Buffer b) in
    let frag = function
    | E (tag, childs) -> `El (tag, childs)
    | D d -> `Data d
    in
    List.iter (fun tree -> Xmlm.output_doc_tree frag output (None, tree)) trees;
    Buffer.contents b
  with
    Xmlm.Error ((line, col), error) ->
      let msg = Printf.sprintf "Line %d, column %d: %s"
        line col (Xmlm.error_message error)
      in
      failwith msg
;;


(** {2 Constants defined in
  {{:http://www.w3.org/TR/rdf-syntax-grammar/}RDF grammar}. } *)

let rdf_ s = Rdf_uri.uri ("http://www.w3.org/1999/02/22-rdf-syntax-ns#"^s);;

(** {3 Syntax names} *)
let rdf_about = rdf_"about";;
let rdf_datatype = rdf_"datatype";;
let rdf_Description = rdf_"Description";;
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
  { subject : Rdf_node.node option ;
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
    prerr_endline (Printf.sprintf "blank_id for %s not found, forging one" id);
    let bid = g.new_blank_id () in
    let gstate = { (*gstate with*) blanks = SMap.add id bid gstate.blanks } in
    (Blank_ bid, gstate)


let rec input_node g state gstate t =
  let state = update_state state t in
  match t with
    D s when state.predicate = None ->
      let msg = Printf.sprintf "Found (Data %S) with no current predicate." s in
      error msg
  | D s ->
      let obj = Rdf_node.node_of_literal_string ?typ: state.datatype ?lang: state.xml_lang s in
      let sub = match state.subject with None -> assert false | Some s -> s in
      let pred = match state.predicate with None -> assert false | Some u -> Uri u in
      g.add_triple ~sub ~pred ~obj;
      gstate
  | E (((pref,s), atts), children) ->
      let (node, gstate) =
        match get_att_uri rdf_about atts with
          Some s -> (Uri (Rdf_uri.uri s), gstate)
        | None ->
            match get_att_uri rdf_ID atts with
              Some id -> (Uri (Rdf_uri.uri ((Rdf_uri.string state.xml_base)^"#"^id)), gstate)
            | None ->
                match get_att_uri rdf_nodeID atts with
                  Some id -> get_blank_node g gstate id
                | None -> (Blank_ (g.new_blank_id ()), gstate)
      in
      begin
        match state.subject, state.predicate with
          Some sub, Some pred ->
            g.add_triple ~sub ~pred: (Uri pred) ~obj: node
        | _ -> ()
      end;
      let state = { state with subject = Some node ; predicate = None } in
      (* add a type arc if the node is not introduced with rdf:Description *)
      if not (is_element rdf_Description (pref,s)) then
        begin
          let type_uri = Rdf_uri.uri (pref^s) in
          g.add_triple ~sub: node ~pred: (Uri rdf_type) ~obj: (Uri type_uri)
        end;
      (* all remaining attributes define triples with literal object values *)
      let f ((pref, s), v) =
        if pref <> Xmlm.ns_xml && pref <> Xmlm.ns_xmlns then
          begin
            let uri_prop = Rdf_uri.uri (pref^s) in
            if not (List.exists (Rdf_uri.equal uri_prop) [ rdf_about ; rdf_ID ; rdf_nodeID ]) then
              begin
                let obj = Rdf_node.node_of_literal_string ?lang: state.xml_lang v in
                g.add_triple ~sub: node ~pred: (Uri uri_prop) ~obj
              end
          end
      in
      List.iter f atts;
      List.fold_left (input_prop g state) gstate children

(* FIXME: handle rdf:li *)
and input_prop g state gstate t =
  let state = update_state state t in
  match t with
    D s ->
      let msg = Printf.sprintf "Found (Data %S) but expected a property node." s in
      error msg
  | E (((pref,s),atts),children) ->
      let sub = match state.subject with None -> assert false | Some sub -> sub in
      let prop_uri = Rdf_uri.uri (pref^s) in
      match get_att_uri rdf_resource atts with
        Some s ->
          let obj = Uri (Rdf_uri.uri s) in
          g.add_triple ~sub ~pred: (Uri prop_uri) ~obj ;
          gstate
      | None ->
          match get_att_uri rdf_nodeID atts with
            Some id ->
              let (obj, gstate) = get_blank_node g gstate id in
              g.add_triple ~sub ~pred: (Uri prop_uri) ~obj ;
              gstate
          | None ->
          match get_att_uri rdf_parseType atts with
            Some "Literal" ->
              let xml = string_of_xmls children in
              let obj = Rdf_node.node_of_literal_string ~typ: rdf_XMLLiteral xml in
              g.add_triple ~sub ~pred: (Uri prop_uri) ~obj;
              gstate
          | Some "Resource" ->
              begin
                 let node = Blank_ (g.new_blank_id ()) in
                 g.add_triple ~sub ~pred: (Uri prop_uri) ~obj: node ;
                 let state = { state with subject = Some node ; predicate = None } in
                 List.fold_left (input_prop g state) gstate children
              end
          | Some s -> error (Printf.sprintf "Unknown parseType %S" s)
          | None ->
              match get_att_uri rdf_datatype atts, children with
              | Some s, [D lit] ->
                  let typ = Rdf_uri.uri s in
                  let obj = Rdf_node.node_of_literal_string ~typ ?lang: state.xml_lang lit in
                  g.add_triple ~sub ~pred: (Uri prop_uri) ~obj;
                  gstate
              | Some s, _ ->
                  let msg = Printf.sprintf "Property %S with datatype %S but no data"
                    (Rdf_uri.string prop_uri) s
                  in
                  error msg
              | None, _ ->
                  (* if we have other attributes than the ones filtered above, they
                    are property relations, with ommited blank nodes *)
                  let pred ((pref,s),v) = pref <> Xmlm.ns_xml && pref <> Xmlm.ns_xmlns in
                  match List.filter pred atts with
                    [] ->
                      let state = { state with predicate = Some prop_uri } in
                      List.fold_left (input_node g state) gstate children
                  | l ->
                      let node = Blank_ (g.new_blank_id ()) in
                      g.add_triple ~sub ~pred: (Uri prop_uri) ~obj: node ;
                      let f ((pref,s),lit) =
                        let obj = Rdf_node.node_of_literal_string ?lang: state.xml_lang lit in
                        let uri_prop = Rdf_uri.uri (pref^s) in
                        g.add_triple ~sub: node ~pred: (Uri uri_prop) ~obj
                      in
                      List.iter f l;
                      gstate
;;

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
        (*let xml = string_of_xmls children in
        prerr_endline xml;*)

        List.fold_left (input_node g state) gstate children
    | t -> input_node g state gstate t
  in
  ignore(gstate)
;;

let input_file g ~base file =
  let ic = open_in file in
  let i = Xmlm.make_input ~strip: true (`Channel ic) in
  let (_,tree) =
    try let t = in_tree i in close_in ic; t
    with e -> close_in ic; raise e
  in

  input g ~base tree
;;

(** {2 Output} *)

let output g = failwith "Not implemented";;
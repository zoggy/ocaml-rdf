(** Reading and writing RDF/XML. *)

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

let rdf_ s = "http://www.w3.org/1999/02/22-rdf-syntax-ns#";;

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
let rdf_n n = rdf_"_"^(string_of_int n);;

(** {3 Resource names} *)
let rdf_nil = rdf_"nil"

(** {2 Input} *)

type state =
  { subject : Rdf_uri.uri option ;
    predicate : Rdf_uri.uri option ;
    xml_base : Rdf_uri.uri option ;
    xml_lang : string option ;
  }

let rec input_node g state t = ()

and input_prop g state t = ()

let input g t =
  match t with
    D _ -> assert false
  | E _ -> assert false

let input_file g file =
  let ic = open_in file in
  let i = Xmlm.make_input (`Channel ic) in
  let (_,tree) =
    try let t = in_tree i in close_in ic; t
    with e -> close_in ic; raise e
  in
  input g tree
;;

(** {2 Output} *)

let output g = failwith "Not implemented";;
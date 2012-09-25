(** Rdf element, as defined in
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

(** {3 Resource names} *)
let rdf_nil = rdf_"nil"
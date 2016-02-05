
let rdf_str = "http://www.w3.org/1999/02/22-rdf-syntax-ns#";;
let rdf = Iri.of_string rdf_str ;;
let rdf_ s = Iri.of_string (rdf_str ^ s);;

let xsd_str = "http://www.w3.org/2001/XMLSchema#" ;;
let xsd = Iri.of_string xsd_str ;;
let xsd_ s = Iri.of_string (xsd_str^s) ;;

let about = rdf_"about";;
let id = rdf_"ID";;
let nodeID = rdf_"nodeID";;
let datatype = rdf_"datatype";;
let description = rdf_"Description";;
let li = rdf_"li";;
let rdf = rdf_"RDF";;
let parseType = rdf_"parseType";;
let resource = rdf_"resource";;

let alt = rdf_ "Alt" ;;
let bag = rdf_ "Bag" ;;
let first = rdf_ "first" ;;
let hTML = rdf_ "HTML" ;;
let langString = rdf_ "langString" ;;
let list = rdf_ "List" ;;
let object_ = rdf_ "object" ;;
let plainLiteral = rdf_ "PlainLiteral" ;;
let predicate = rdf_ "predicate" ;;
let property = rdf_ "Property" ;;
let rest = rdf_ "rest" ;;
let seq = rdf_ "Seq" ;;
let statement = rdf_ "Statement" ;;
let subject = rdf_ "subject" ;;
let type_ = rdf_ "type" ;;
let value = rdf_ "value" ;;
let xmlLiteral = rdf_ "XMLLiteral" ;;

let n n = rdf_("_"^(string_of_int n));;
let nil = rdf_"nil"

let xsd_integer = xsd_"integer";;
let xsd_double = xsd_"double";;
let xsd_decimal = xsd_"decimal";;
let xsd_boolean = xsd_"boolean";;
let xsd_string = xsd_"string";;
let xsd_datetime = xsd_"dateTime";;

module Open = struct
    let rdf_about = about
    let rdf_id = id
    let rdf_datatype = datatype
    let rdf_description = description
    let rdf_li = li
    let rdf_nodeID = nodeID
    let rdf_rdf = rdf
    let rdf_parseType = parseType
    let rdf_resource = resource

    let rdf_alt = alt
    let rdf_bag = bag
    let rdf_first = first
    let rdf_hTML = hTML
    let rdf_langString = langString
    let rdf_list = list
    let rdf_object = object_
    let rdf_plainLiteral = plainLiteral
    let rdf_predicate = predicate
    let rdf_property = property
    let rdf_rest = rest
    let rdf_seq = seq
    let rdf_statement = statement
    let rdf_subject = subject
    let rdf_type = type_
    let rdf_value = value
    let rdf_xmlLiteral = xmlLiteral

    let rdf_n = n
    let rdf_nil = nil

    let xsd_integer = xsd_integer
    let xsd_double = xsd_double
    let xsd_decimal = xsd_decimal
    let xsd_boolean = xsd_boolean
    let xsd_string = xsd_string
    let xsd_datetime = xsd_datetime
end

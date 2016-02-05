
let rdf_str = "http://www.w3.org/1999/02/22-rdf-syntax-ns#";;
let rdf = Iri.of_string rdf_str ;;
let rdf_ s = Iri.of_string (rdf_str ^ s);;

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
let xMLLiteral = rdf_ "XMLLiteral" ;;

module Open = struct
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
  let rdf_xMLLiteral = xMLLiteral
end

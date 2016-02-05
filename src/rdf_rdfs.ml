
let rdfs_str = "http://www.w3.org/2000/01/rdf-schema#";;
let rdfs = Iri.of_string rdfs_str ;;
let rdfs_ s = Iri.of_string (rdfs_str ^ s);;

let class_ = rdfs_ "Class" ;;
let comment = rdfs_ "comment" ;;
let container = rdfs_ "Container" ;;
let containerMembershipProperty = rdfs_ "ContainerMembershipProperty" ;;
let datatype = rdfs_ "Datatype" ;;
let domain = rdfs_ "domain" ;;
let isDefinedBy = rdfs_ "isDefinedBy" ;;
let label = rdfs_ "label" ;;
let literal = rdfs_ "Literal" ;;
let member = rdfs_ "member" ;;
let range = rdfs_ "range" ;;
let resource = rdfs_ "Resource" ;;
let seeAlso = rdfs_ "seeAlso" ;;
let subClassOf = rdfs_ "subClassOf" ;;
let subPropertyOf = rdfs_ "subPropertyOf" ;;

module Open = struct
  let rdfs_class = class_
  let rdfs_comment = comment
  let rdfs_container = container
  let rdfs_containerMembershipProperty = containerMembershipProperty
  let rdfs_datatype = datatype
  let rdfs_domain = domain
  let rdfs_isDefinedBy = isDefinedBy
  let rdfs_label = label
  let rdfs_literal = literal
  let rdfs_member = member
  let rdfs_range = range
  let rdfs_resource = resource
  let rdfs_seeAlso = seeAlso
  let rdfs_subClassOf = subClassOf
  let rdfs_subPropertyOf = subPropertyOf
end

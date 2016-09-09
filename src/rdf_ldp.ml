
let ldp_str = "http://www.w3.org/ns/ldp#";;
let ldp = Iri.of_string ldp_str ;;
let ldp_ s = Iri.of_string (ldp_str ^ s);;

let c_BasicContainer = ldp_ "BasicContainer" ;;
let c_Container = ldp_ "Container" ;;
let c_DirectContainer = ldp_ "DirectContainer" ;;
let c_IndirectContainer = ldp_ "IndirectContainer" ;;
let c_NonRDFSource = ldp_ "NonRDFSource" ;;
let c_Page = ldp_ "Page" ;;
let c_PageSortCriterion = ldp_ "PageSortCriterion" ;;
let c_RDFSource = ldp_ "RDFSource" ;;
let c_Resource = ldp_ "Resource" ;;
let constrainedBy = ldp_ "constrainedBy" ;;
let contains = ldp_ "contains" ;;
let hasMemberRelation = ldp_ "hasMemberRelation" ;;
let insertedContentRelation = ldp_ "insertedContentRelation" ;;
let isMemberOfRelation = ldp_ "isMemberOfRelation" ;;
let member = ldp_ "member" ;;
let membershipResource = ldp_ "membershipResource" ;;
let pageSequence = ldp_ "pageSequence" ;;
let pageSortCollation = ldp_ "pageSortCollation" ;;
let pageSortCriteria = ldp_ "pageSortCriteria" ;;
let pageSortOrder = ldp_ "pageSortOrder" ;;
let pageSortPredicate = ldp_ "pageSortPredicate" ;;

module Open = struct
  let ldp_c_BasicContainer = c_BasicContainer
  let ldp_c_Container = c_Container
  let ldp_c_DirectContainer = c_DirectContainer
  let ldp_c_IndirectContainer = c_IndirectContainer
  let ldp_c_NonRDFSource = c_NonRDFSource
  let ldp_c_Page = c_Page
  let ldp_c_PageSortCriterion = c_PageSortCriterion
  let ldp_c_RDFSource = c_RDFSource
  let ldp_c_Resource = c_Resource
  let ldp_constrainedBy = constrainedBy
  let ldp_contains = contains
  let ldp_hasMemberRelation = hasMemberRelation
  let ldp_insertedContentRelation = insertedContentRelation
  let ldp_isMemberOfRelation = isMemberOfRelation
  let ldp_member = member
  let ldp_membershipResource = membershipResource
  let ldp_pageSequence = pageSequence
  let ldp_pageSortCollation = pageSortCollation
  let ldp_pageSortCriteria = pageSortCriteria
  let ldp_pageSortOrder = pageSortOrder
  let ldp_pageSortPredicate = pageSortPredicate
end

class from ?sub g =
  let sub = match sub with None -> g.Rdf_graph.name() | Some iri -> iri in
  let sub = Rdf_term.Iri sub in
  let get_prop_list pred =
    Rdf_graph.iri_objects_of g ~sub ~pred
  in
  object
  method constrainedBy = get_prop_list constrainedBy
  method contains = get_prop_list contains
  method hasMemberRelation = get_prop_list hasMemberRelation
  method insertedContentRelation = get_prop_list insertedContentRelation
  method isMemberOfRelation = get_prop_list isMemberOfRelation
  method member = get_prop_list member
  method membershipResource = get_prop_list membershipResource
  method pageSequence = get_prop_list pageSequence
  method pageSortCollation = get_prop_list pageSortCollation
  method pageSortCriteria = get_prop_list pageSortCriteria
  method pageSortOrder = get_prop_list pageSortOrder
  method pageSortPredicate = get_prop_list pageSortPredicate
  end

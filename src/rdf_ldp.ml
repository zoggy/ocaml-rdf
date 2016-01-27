
let ldp_str = "http://www.w3.org/ns/ldp#";;
let ldp = Iri.of_string ldp_str ;;
let ldp_ s = Iri.of_string (ldp_str ^ s);;

let ldp_BasicContainer = ldp_"BasicContainer" ;;

let ldp_constrainedBy = ldp_"constrainedBy" ;;

let ldp_Container = ldp_"Container" ;;

let ldp_contains = ldp_"contains" ;;

let ldp_DirectContainer = ldp_"DirectContainer" ;;

let ldp_hasMemberRelation = ldp_"hasMemberRelation" ;;

let ldp_IndirectContainer = ldp_"IndirectContainer" ;;

let ldp_insertedContentRelation = ldp_"insertedContentRelation" ;;

let ldp_isMemberOfRelation = ldp_"isMemberOfRelation" ;;

let ldp_member = ldp_"member" ;;

let ldp_membershipResource = ldp_"membershipResource" ;;

let ldp_NonRDFSource = ldp_"NonRDFSource" ;;

let ldp_Page = ldp_"Page" ;;

let ldp_pageSequence = ldp_"pageSequence" ;;

let ldp_pageSortCollation = ldp_"pageSortCollation" ;;

let ldp_pageSortCriteria = ldp_"pageSortCriteria" ;;

let ldp_PageSortCriterion = ldp_"PageSortCriterion" ;;

let ldp_pageSortOrder = ldp_"pageSortOrder" ;;

let ldp_pageSortPredicate = ldp_"pageSortPredicate" ;;

let ldp_RDFSource = ldp_"RDFSource" ;;

let ldp_Resource = ldp_"Resource" ;;


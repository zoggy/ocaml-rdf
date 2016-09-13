
let earl_str = "http://www.w3.org/ns/earl#";;
let earl = Iri.of_string earl_str ;;
let earl_ s = Iri.of_string (earl_str ^ s);;

let c_Assertion = earl_ "Assertion" ;;
let c_Assertor = earl_ "Assertor" ;;
let c_CannotTell = earl_ "CannotTell" ;;
let c_Fail = earl_ "Fail" ;;
let c_NotApplicable = earl_ "NotApplicable" ;;
let c_NotTested = earl_ "NotTested" ;;
let c_OutcomeValue = earl_ "OutcomeValue" ;;
let c_Pass = earl_ "Pass" ;;
let c_Software = earl_ "Software" ;;
let c_TestCase = earl_ "TestCase" ;;
let c_TestCriterion = earl_ "TestCriterion" ;;
let c_TestMode = earl_ "TestMode" ;;
let c_TestRequirement = earl_ "TestRequirement" ;;
let c_TestResult = earl_ "TestResult" ;;
let c_TestSubject = earl_ "TestSubject" ;;
let assertedBy = earl_ "assertedBy" ;;
let info = earl_ "info" ;;
let mainAssertor = earl_ "mainAssertor" ;;
let mode = earl_ "mode" ;;
let outcome = earl_ "outcome" ;;
let pointer = earl_ "pointer" ;;
let result = earl_ "result" ;;
let subject = earl_ "subject" ;;
let test = earl_ "test" ;;

module Open = struct
  let earl_c_Assertion = c_Assertion
  let earl_c_Assertor = c_Assertor
  let earl_c_CannotTell = c_CannotTell
  let earl_c_Fail = c_Fail
  let earl_c_NotApplicable = c_NotApplicable
  let earl_c_NotTested = c_NotTested
  let earl_c_OutcomeValue = c_OutcomeValue
  let earl_c_Pass = c_Pass
  let earl_c_Software = c_Software
  let earl_c_TestCase = c_TestCase
  let earl_c_TestCriterion = c_TestCriterion
  let earl_c_TestMode = c_TestMode
  let earl_c_TestRequirement = c_TestRequirement
  let earl_c_TestResult = c_TestResult
  let earl_c_TestSubject = c_TestSubject
  let earl_assertedBy = assertedBy
  let earl_info = info
  let earl_mainAssertor = mainAssertor
  let earl_mode = mode
  let earl_outcome = outcome
  let earl_pointer = pointer
  let earl_result = result
  let earl_subject = subject
  let earl_test = test
end

class from ?sub g =
  let sub = match sub with None -> g.Rdf_graph.name() | Some iri -> iri in
  let sub = Rdf_term.Iri sub in
  object
  method assertedBy = Rdf_graph.iri_objects_of g ~sub ~pred: assertedBy
  method info = Rdf_graph.literal_objects_of g ~sub ~pred: info
  method mainAssertor = Rdf_graph.iri_objects_of g ~sub ~pred: mainAssertor
  method mode = Rdf_graph.iri_objects_of g ~sub ~pred: mode
  method outcome = Rdf_graph.iri_objects_of g ~sub ~pred: outcome
  method pointer = Rdf_graph.iri_objects_of g ~sub ~pred: pointer
  method result = Rdf_graph.iri_objects_of g ~sub ~pred: result
  method subject = Rdf_graph.iri_objects_of g ~sub ~pred: subject
  method test = Rdf_graph.iri_objects_of g ~sub ~pred: test
  end

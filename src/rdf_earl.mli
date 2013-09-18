(** Elements of [http://www.w3.org/ns/earl#] *)

(** [http://www.w3.org/ns/earl#] *)
val earl : Rdf_uri.uri
val earl_ : string -> Rdf_uri.uri

(** assertor of an assertion *)
val earl_assertedBy : Rdf_uri.uri

(** a statement that embodies the results of a test *)
val earl_Assertion : Rdf_uri.uri

(** an entity such as a person, a software tool, an organization, or any other grouping that carries out a test collectively *)
val earl_Assertor : Rdf_uri.uri

(** the class of outcomes to denote an undetermined outcome *)
val earl_CannotTell : Rdf_uri.uri

(** the class of outcomes to denote failing a test *)
val earl_Fail : Rdf_uri.uri

(** additional warnings or error messages in a human-readable form *)
val earl_info : Rdf_uri.uri

(** assertor that is primarily responsible for performing the test *)
val earl_mainAssertor : Rdf_uri.uri

(** mode in which the test was performed *)
val earl_mode : Rdf_uri.uri

(** the class of outcomes to denote the test is not applicable *)
val earl_NotApplicable : Rdf_uri.uri

(** the class of outcomes to denote the test has not been carried out *)
val earl_NotTested : Rdf_uri.uri

(** outcome of performing the test *)
val earl_outcome : Rdf_uri.uri

(** a discrete value that describes a resulting condition from carrying out the test *)
val earl_OutcomeValue : Rdf_uri.uri

(** the class of outcomes to denote passing a test *)
val earl_Pass : Rdf_uri.uri

(** location within a test subject that are most relevant to a test result *)
val earl_pointer : Rdf_uri.uri

(** result of an assertion *)
val earl_result : Rdf_uri.uri

(** any piece of software such as an authoring tool, browser, or evaluation tool *)
val earl_Software : Rdf_uri.uri

(** test subject of an assertion *)
val earl_subject : Rdf_uri.uri

(** test criterion of an assertion *)
val earl_test : Rdf_uri.uri

(** an atomic test, usually one that is a partial test for a requirement *)
val earl_TestCase : Rdf_uri.uri

(** a testable statement, usually one that can be passed or failed *)
val earl_TestCriterion : Rdf_uri.uri

(** describes how a test was carried out *)
val earl_TestMode : Rdf_uri.uri

(** a higher-level requirement that is tested by executing one or more sub-tests *)
val earl_TestRequirement : Rdf_uri.uri

(** the actual result of performing the test *)
val earl_TestResult : Rdf_uri.uri

(** the class of things that have been tested against some test criterion *)
val earl_TestSubject : Rdf_uri.uri


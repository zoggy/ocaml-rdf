(** Elements of [http://www.w3.org/ns/earl#] *)

(** [http://www.w3.org/ns/earl#] *)
val earl : Iri.t
val earl_ : string -> Iri.t

(** a statement that embodies the results of a test *)
val c_Assertion : Iri.t

(** an entity such as a person, a software tool, an organization, or any other grouping that carries out a test collectively *)
val c_Assertor : Iri.t

(** the class of outcomes to denote an undetermined outcome *)
val c_CannotTell : Iri.t

(** the class of outcomes to denote failing a test *)
val c_Fail : Iri.t

(** the class of outcomes to denote the test is not applicable *)
val c_NotApplicable : Iri.t

(** the class of outcomes to denote the test has not been carried out *)
val c_NotTested : Iri.t

(** a discrete value that describes a resulting condition from carrying out the test *)
val c_OutcomeValue : Iri.t

(** the class of outcomes to denote passing a test *)
val c_Pass : Iri.t

(** any piece of software such as an authoring tool, browser, or evaluation tool *)
val c_Software : Iri.t

(** an atomic test, usually one that is a partial test for a requirement *)
val c_TestCase : Iri.t

(** a testable statement, usually one that can be passed or failed *)
val c_TestCriterion : Iri.t

(** describes how a test was carried out *)
val c_TestMode : Iri.t

(** a higher-level requirement that is tested by executing one or more sub-tests *)
val c_TestRequirement : Iri.t

(** the actual result of performing the test *)
val c_TestResult : Iri.t

(** the class of things that have been tested against some test criterion *)
val c_TestSubject : Iri.t

(** assertor of an assertion *)
val assertedBy : Iri.t

(** additional warnings or error messages in a human-readable form *)
val info : Iri.t

(** assertor that is primarily responsible for performing the test *)
val mainAssertor : Iri.t

(** mode in which the test was performed *)
val mode : Iri.t

(** outcome of performing the test *)
val outcome : Iri.t

(** location within a test subject that are most relevant to a test result *)
val pointer : Iri.t

(** result of an assertion *)
val result : Iri.t

(** test subject of an assertion *)
val subject : Iri.t

(** test criterion of an assertion *)
val test : Iri.t


module Open : sig
  (** a statement that embodies the results of a test *)
  val earl_c_Assertion : Iri.t

  (** an entity such as a person, a software tool, an organization, or any other grouping that carries out a test collectively *)
  val earl_c_Assertor : Iri.t

  (** the class of outcomes to denote an undetermined outcome *)
  val earl_c_CannotTell : Iri.t

  (** the class of outcomes to denote failing a test *)
  val earl_c_Fail : Iri.t

  (** the class of outcomes to denote the test is not applicable *)
  val earl_c_NotApplicable : Iri.t

  (** the class of outcomes to denote the test has not been carried out *)
  val earl_c_NotTested : Iri.t

  (** a discrete value that describes a resulting condition from carrying out the test *)
  val earl_c_OutcomeValue : Iri.t

  (** the class of outcomes to denote passing a test *)
  val earl_c_Pass : Iri.t

  (** any piece of software such as an authoring tool, browser, or evaluation tool *)
  val earl_c_Software : Iri.t

  (** an atomic test, usually one that is a partial test for a requirement *)
  val earl_c_TestCase : Iri.t

  (** a testable statement, usually one that can be passed or failed *)
  val earl_c_TestCriterion : Iri.t

  (** describes how a test was carried out *)
  val earl_c_TestMode : Iri.t

  (** a higher-level requirement that is tested by executing one or more sub-tests *)
  val earl_c_TestRequirement : Iri.t

  (** the actual result of performing the test *)
  val earl_c_TestResult : Iri.t

  (** the class of things that have been tested against some test criterion *)
  val earl_c_TestSubject : Iri.t

  (** assertor of an assertion *)
  val earl_assertedBy : Iri.t

  (** additional warnings or error messages in a human-readable form *)
  val earl_info : Iri.t

  (** assertor that is primarily responsible for performing the test *)
  val earl_mainAssertor : Iri.t

  (** mode in which the test was performed *)
  val earl_mode : Iri.t

  (** outcome of performing the test *)
  val earl_outcome : Iri.t

  (** location within a test subject that are most relevant to a test result *)
  val earl_pointer : Iri.t

  (** result of an assertion *)
  val earl_result : Iri.t

  (** test subject of an assertion *)
  val earl_subject : Iri.t

  (** test criterion of an assertion *)
  val earl_test : Iri.t

end

class from : ?sub: Iri.t -> Rdf_graph.graph ->
  object
    method assertedBy : Iri.t list
    method info : Rdf_term.literal list
    method mainAssertor : Iri.t list
    method mode : Iri.t list
    method outcome : Iri.t list
    method pointer : Iri.t list
    method result : Iri.t list
    method subject : Iri.t list
    method test : Iri.t list
  end

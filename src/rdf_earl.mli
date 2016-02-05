(** Elements of [http://www.w3.org/ns/earl#] *)

(** [http://www.w3.org/ns/earl#] *)
val earl : Iri.t
val earl_ : string -> Iri.t

(** assertor of an assertion *)
val assertedBy : Iri.t

(** a statement that embodies the results of a test *)
val assertion : Iri.t

(** an entity such as a person, a software tool, an organization, or any other grouping that carries out a test collectively *)
val assertor : Iri.t

(** the class of outcomes to denote an undetermined outcome *)
val cannotTell : Iri.t

(** the class of outcomes to denote failing a test *)
val fail : Iri.t

(** additional warnings or error messages in a human-readable form *)
val info : Iri.t

(** assertor that is primarily responsible for performing the test *)
val mainAssertor : Iri.t

(** mode in which the test was performed *)
val mode : Iri.t

(** the class of outcomes to denote the test is not applicable *)
val notApplicable : Iri.t

(** the class of outcomes to denote the test has not been carried out *)
val notTested : Iri.t

(** outcome of performing the test *)
val outcome : Iri.t

(** a discrete value that describes a resulting condition from carrying out the test *)
val outcomeValue : Iri.t

(** the class of outcomes to denote passing a test *)
val pass : Iri.t

(** location within a test subject that are most relevant to a test result *)
val pointer : Iri.t

(** result of an assertion *)
val result : Iri.t

(** any piece of software such as an authoring tool, browser, or evaluation tool *)
val software : Iri.t

(** test subject of an assertion *)
val subject : Iri.t

(** test criterion of an assertion *)
val test : Iri.t

(** an atomic test, usually one that is a partial test for a requirement *)
val testCase : Iri.t

(** a testable statement, usually one that can be passed or failed *)
val testCriterion : Iri.t

(** describes how a test was carried out *)
val testMode : Iri.t

(** a higher-level requirement that is tested by executing one or more sub-tests *)
val testRequirement : Iri.t

(** the actual result of performing the test *)
val testResult : Iri.t

(** the class of things that have been tested against some test criterion *)
val testSubject : Iri.t


module Open : sig
  (** assertor of an assertion *)
  val earl_assertedBy : Iri.t

  (** a statement that embodies the results of a test *)
  val earl_assertion : Iri.t

  (** an entity such as a person, a software tool, an organization, or any other grouping that carries out a test collectively *)
  val earl_assertor : Iri.t

  (** the class of outcomes to denote an undetermined outcome *)
  val earl_cannotTell : Iri.t

  (** the class of outcomes to denote failing a test *)
  val earl_fail : Iri.t

  (** additional warnings or error messages in a human-readable form *)
  val earl_info : Iri.t

  (** assertor that is primarily responsible for performing the test *)
  val earl_mainAssertor : Iri.t

  (** mode in which the test was performed *)
  val earl_mode : Iri.t

  (** the class of outcomes to denote the test is not applicable *)
  val earl_notApplicable : Iri.t

  (** the class of outcomes to denote the test has not been carried out *)
  val earl_notTested : Iri.t

  (** outcome of performing the test *)
  val earl_outcome : Iri.t

  (** a discrete value that describes a resulting condition from carrying out the test *)
  val earl_outcomeValue : Iri.t

  (** the class of outcomes to denote passing a test *)
  val earl_pass : Iri.t

  (** location within a test subject that are most relevant to a test result *)
  val earl_pointer : Iri.t

  (** result of an assertion *)
  val earl_result : Iri.t

  (** any piece of software such as an authoring tool, browser, or evaluation tool *)
  val earl_software : Iri.t

  (** test subject of an assertion *)
  val earl_subject : Iri.t

  (** test criterion of an assertion *)
  val earl_test : Iri.t

  (** an atomic test, usually one that is a partial test for a requirement *)
  val earl_testCase : Iri.t

  (** a testable statement, usually one that can be passed or failed *)
  val earl_testCriterion : Iri.t

  (** describes how a test was carried out *)
  val earl_testMode : Iri.t

  (** a higher-level requirement that is tested by executing one or more sub-tests *)
  val earl_testRequirement : Iri.t

  (** the actual result of performing the test *)
  val earl_testResult : Iri.t

  (** the class of things that have been tested against some test criterion *)
  val earl_testSubject : Iri.t

end

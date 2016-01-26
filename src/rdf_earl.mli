(*********************************************************************************)
(*                OCaml-RDF                                                      *)
(*                                                                               *)
(*    Copyright (C) 2012-2016 Institut National de Recherche en Informatique     *)
(*    et en Automatique. All rights reserved.                                    *)
(*                                                                               *)
(*    This program is free software; you can redistribute it and/or modify       *)
(*    it under the terms of the GNU Lesser General Public License version        *)
(*    3 as published by the Free Software Foundation.                            *)
(*                                                                               *)
(*    This program is distributed in the hope that it will be useful,            *)
(*    but WITHOUT ANY WARRANTY; without even the implied warranty of             *)
(*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the              *)
(*    GNU General Public License for more details.                               *)
(*                                                                               *)
(*    You should have received a copy of the GNU General Public License          *)
(*    along with this program; if not, write to the Free Software                *)
(*    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA                   *)
(*    02111-1307  USA                                                            *)
(*                                                                               *)
(*    Contact: Maxence.Guesdon@inria.fr                                          *)
(*                                                                               *)
(*********************************************************************************)

(** Elements of [http://www.w3.org/ns/earl#] *)

(** [http://www.w3.org/ns/earl#] *)
val earl : Iri.t
val earl_ : string -> Iri.t

(** assertor of an assertion *)
val earl_assertedBy : Iri.t

(** a statement that embodies the results of a test *)
val earl_Assertion : Iri.t

(** an entity such as a person, a software tool, an organization, or any other grouping that carries out a test collectively *)
val earl_Assertor : Iri.t

(** the class of outcomes to denote an undetermined outcome *)
val earl_CannotTell : Iri.t

(** the class of outcomes to denote failing a test *)
val earl_Fail : Iri.t

(** additional warnings or error messages in a human-readable form *)
val earl_info : Iri.t

(** assertor that is primarily responsible for performing the test *)
val earl_mainAssertor : Iri.t

(** mode in which the test was performed *)
val earl_mode : Iri.t

(** the class of outcomes to denote the test is not applicable *)
val earl_NotApplicable : Iri.t

(** the class of outcomes to denote the test has not been carried out *)
val earl_NotTested : Iri.t

(** outcome of performing the test *)
val earl_outcome : Iri.t

(** a discrete value that describes a resulting condition from carrying out the test *)
val earl_OutcomeValue : Iri.t

(** the class of outcomes to denote passing a test *)
val earl_Pass : Iri.t

(** location within a test subject that are most relevant to a test result *)
val earl_pointer : Iri.t

(** result of an assertion *)
val earl_result : Iri.t

(** any piece of software such as an authoring tool, browser, or evaluation tool *)
val earl_Software : Iri.t

(** test subject of an assertion *)
val earl_subject : Iri.t

(** test criterion of an assertion *)
val earl_test : Iri.t

(** an atomic test, usually one that is a partial test for a requirement *)
val earl_TestCase : Iri.t

(** a testable statement, usually one that can be passed or failed *)
val earl_TestCriterion : Iri.t

(** describes how a test was carried out *)
val earl_TestMode : Iri.t

(** a higher-level requirement that is tested by executing one or more sub-tests *)
val earl_TestRequirement : Iri.t

(** the actual result of performing the test *)
val earl_TestResult : Iri.t

(** the class of things that have been tested against some test criterion *)
val earl_TestSubject : Iri.t


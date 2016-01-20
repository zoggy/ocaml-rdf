(*********************************************************************************)
(*                OCaml-RDF                                                      *)
(*                                                                               *)
(*    Copyright (C) 2012-2014 Institut National de Recherche en Informatique     *)
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
val earl : Iri.iri
val earl_ : string -> Iri.iri

(** assertor of an assertion *)
val earl_assertedBy : Iri.iri

(** a statement that embodies the results of a test *)
val earl_Assertion : Iri.iri

(** an entity such as a person, a software tool, an organization, or any other grouping that carries out a test collectively *)
val earl_Assertor : Iri.iri

(** the class of outcomes to denote an undetermined outcome *)
val earl_CannotTell : Iri.iri

(** the class of outcomes to denote failing a test *)
val earl_Fail : Iri.iri

(** additional warnings or error messages in a human-readable form *)
val earl_info : Iri.iri

(** assertor that is primarily responsible for performing the test *)
val earl_mainAssertor : Iri.iri

(** mode in which the test was performed *)
val earl_mode : Iri.iri

(** the class of outcomes to denote the test is not applicable *)
val earl_NotApplicable : Iri.iri

(** the class of outcomes to denote the test has not been carried out *)
val earl_NotTested : Iri.iri

(** outcome of performing the test *)
val earl_outcome : Iri.iri

(** a discrete value that describes a resulting condition from carrying out the test *)
val earl_OutcomeValue : Iri.iri

(** the class of outcomes to denote passing a test *)
val earl_Pass : Iri.iri

(** location within a test subject that are most relevant to a test result *)
val earl_pointer : Iri.iri

(** result of an assertion *)
val earl_result : Iri.iri

(** any piece of software such as an authoring tool, browser, or evaluation tool *)
val earl_Software : Iri.iri

(** test subject of an assertion *)
val earl_subject : Iri.iri

(** test criterion of an assertion *)
val earl_test : Iri.iri

(** an atomic test, usually one that is a partial test for a requirement *)
val earl_TestCase : Iri.iri

(** a testable statement, usually one that can be passed or failed *)
val earl_TestCriterion : Iri.iri

(** describes how a test was carried out *)
val earl_TestMode : Iri.iri

(** a higher-level requirement that is tested by executing one or more sub-tests *)
val earl_TestRequirement : Iri.iri

(** the actual result of performing the test *)
val earl_TestResult : Iri.iri

(** the class of things that have been tested against some test criterion *)
val earl_TestSubject : Iri.iri


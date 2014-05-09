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
val earl : Rdf_iri.iri
val earl_ : string -> Rdf_iri.iri

(** assertor of an assertion *)
val earl_assertedBy : Rdf_iri.iri

(** a statement that embodies the results of a test *)
val earl_Assertion : Rdf_iri.iri

(** an entity such as a person, a software tool, an organization, or any other grouping that carries out a test collectively *)
val earl_Assertor : Rdf_iri.iri

(** the class of outcomes to denote an undetermined outcome *)
val earl_CannotTell : Rdf_iri.iri

(** the class of outcomes to denote failing a test *)
val earl_Fail : Rdf_iri.iri

(** additional warnings or error messages in a human-readable form *)
val earl_info : Rdf_iri.iri

(** assertor that is primarily responsible for performing the test *)
val earl_mainAssertor : Rdf_iri.iri

(** mode in which the test was performed *)
val earl_mode : Rdf_iri.iri

(** the class of outcomes to denote the test is not applicable *)
val earl_NotApplicable : Rdf_iri.iri

(** the class of outcomes to denote the test has not been carried out *)
val earl_NotTested : Rdf_iri.iri

(** outcome of performing the test *)
val earl_outcome : Rdf_iri.iri

(** a discrete value that describes a resulting condition from carrying out the test *)
val earl_OutcomeValue : Rdf_iri.iri

(** the class of outcomes to denote passing a test *)
val earl_Pass : Rdf_iri.iri

(** location within a test subject that are most relevant to a test result *)
val earl_pointer : Rdf_iri.iri

(** result of an assertion *)
val earl_result : Rdf_iri.iri

(** any piece of software such as an authoring tool, browser, or evaluation tool *)
val earl_Software : Rdf_iri.iri

(** test subject of an assertion *)
val earl_subject : Rdf_iri.iri

(** test criterion of an assertion *)
val earl_test : Rdf_iri.iri

(** an atomic test, usually one that is a partial test for a requirement *)
val earl_TestCase : Rdf_iri.iri

(** a testable statement, usually one that can be passed or failed *)
val earl_TestCriterion : Rdf_iri.iri

(** describes how a test was carried out *)
val earl_TestMode : Rdf_iri.iri

(** a higher-level requirement that is tested by executing one or more sub-tests *)
val earl_TestRequirement : Rdf_iri.iri

(** the actual result of performing the test *)
val earl_TestResult : Rdf_iri.iri

(** the class of things that have been tested against some test criterion *)
val earl_TestSubject : Rdf_iri.iri


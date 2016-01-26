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

let earl_str = "http://www.w3.org/ns/earl#";;
let earl = Iri.of_string earl_str ;;
let earl_ s = Iri.of_string (earl_str ^ s);;

let earl_assertedBy = earl_"assertedBy" ;;

let earl_Assertion = earl_"Assertion" ;;

let earl_Assertor = earl_"Assertor" ;;

let earl_CannotTell = earl_"CannotTell" ;;

let earl_Fail = earl_"Fail" ;;

let earl_info = earl_"info" ;;

let earl_mainAssertor = earl_"mainAssertor" ;;

let earl_mode = earl_"mode" ;;

let earl_NotApplicable = earl_"NotApplicable" ;;

let earl_NotTested = earl_"NotTested" ;;

let earl_outcome = earl_"outcome" ;;

let earl_OutcomeValue = earl_"OutcomeValue" ;;

let earl_Pass = earl_"Pass" ;;

let earl_pointer = earl_"pointer" ;;

let earl_result = earl_"result" ;;

let earl_Software = earl_"Software" ;;

let earl_subject = earl_"subject" ;;

let earl_test = earl_"test" ;;

let earl_TestCase = earl_"TestCase" ;;

let earl_TestCriterion = earl_"TestCriterion" ;;

let earl_TestMode = earl_"TestMode" ;;

let earl_TestRequirement = earl_"TestRequirement" ;;

let earl_TestResult = earl_"TestResult" ;;

let earl_TestSubject = earl_"TestSubject" ;;


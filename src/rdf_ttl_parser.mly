/*********************************************************************************/
/*                OCaml-RDF                                                      */
/*                                                                               */
/*    Copyright (C) 2012-2014 Institut National de Recherche en Informatique     */
/*    et en Automatique. All rights reserved.                                    */
/*                                                                               */
/*    This program is free software; you can redistribute it and/or modify       */
/*    it under the terms of the GNU Lesser General Public License version        */
/*    3 as published by the Free Software Foundation.                            */
/*                                                                               */
/*    This program is distributed in the hope that it will be useful,            */
/*    but WITHOUT ANY WARRANTY; without even the implied warranty of             */
/*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the              */
/*    GNU General Public License for more details.                               */
/*                                                                               */
/*    You should have received a copy of the GNU General Public License          */
/*    along with this program; if not, write to the Free Software                */
/*    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA                   */
/*    02111-1307  USA                                                            */
/*                                                                               */
/*    Contact: Maxence.Guesdon@inria.fr                                          */
/*                                                                               */
/*********************************************************************************/

(** *)

%{
open Rdf_ttl_types

%}
%token SEMICOLON COMMA DOT HATHAT
%token AT_PREFIX AT_BASE
%token PREFIX  BASE
%token A ANON
%token EOF

%token LEFT_PAR RIGHT_PAR
%token LEFT_BRACKET RIGHT_BRACKET

%token <string> Iriref_
%token <string> Identifier
%token <string> At_identifier
%token <string option * string option> Qname_
%token <string> Bname
%token <string> String_
%token <string> Integer
%token <string> Decimal
%token <string> Double
%token <string> Boolean

%start <Rdf_ttl_types.turtle> main

%%

%public main : list(statement) EOF { $1 }

statement:
  directive { Directive $1 }
| triples DOT { let (a,b) = $1 in Triples (a, b) }
;

directive:
  prefixID { $1 }
| base { $1 }
;

prefixID:
| AT_PREFIX n=Identifier iri=iriref DOT
    { Prefix (n, iri) }
| PREFIX n=Identifier iri=iriref
    { Prefix (n, iri) }
;

base:
| AT_BASE iri=iriref DOT
    { Base iri }
| BASE iri=iriref
    { Base iri }
;

triples:
| subject predobjs { ($1, $2) }
| blanknodepropertylist l=option(predobjs)
   { (Sub_blank $1, match l with None -> [] | Some l -> l) }
;

subject:
| iri { Sub_iri $1 }
| blanknode { Sub_blank $1 }
| collection { Sub_blank (Collection $1) }
;

iri:
| iriref { Iriref $1 }
| prefixedName { $1 }
;

prefixedName:
| Qname_ { let (a, b) = $1 in Qname (a, b) }
| Identifier {
    let p = match $1 with "" -> None | s -> Some s in
    Qname (p, None)
  }
;

blanknode:
| Bname { NodeId $1 }
| ANON { Empty }
;

collection: LEFT_PAR list(object_) RIGHT_PAR { $2 }
;

predobjs:
| predobj nonempty_list(SEMICOLON) predobjs { $1 :: $3 }
| predobj list(SEMICOLON) { [ $1 ] }
;

predobj: verb separated_nonempty_list(COMMA, object_) { ($1, $2) }
;

object_:
| iri { Obj_iri $1 }
| blanknode { Obj_blank $1 }
| collection { Obj_blank (Collection $1) }
| blanknodepropertylist { Obj_blank $1 }
| literal { Obj_literal $1 }
;

verb:
| iri { Pred_iri $1 }
| A { Pred_a }
;

literal:
| String_ { String ($1, None, None) }
| String_ lang=at_identifier { String ($1, Some lang, None) }
| String_ dt=datatype { String ($1, None, Some dt) }
| Integer { String ($1, None, Some (Iriref (Rdf_iri.string Rdf_rdf.xsd_integer))) }
| Decimal { String ($1, None, Some (Iriref (Rdf_iri.string Rdf_rdf.xsd_decimal))) }
| Double { String ($1, None, Some (Iriref (Rdf_iri.string Rdf_rdf.xsd_double))) }
| Boolean { String ($1, None, Some (Iriref (Rdf_iri.string Rdf_rdf.xsd_boolean))) }
;

blanknodepropertylist:
| LEFT_BRACKET predobjs RIGHT_BRACKET { PredObjs $2 }
;

at_identifier: At_identifier { $1 }
;

datatype: HATHAT iri { $2 }
;

iriref: iri=Iriref_ { iri }
;

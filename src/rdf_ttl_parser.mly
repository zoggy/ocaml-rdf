/*********************************************************************************/
/*                OCaml-RDF                                                      */
/*                                                                               */
/*    Copyright (C) 2012-2013 Institut National de Recherche en Informatique     */
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
%token SEMICOLON COMMA DOT HATHAT AT
%token AT_PREFIX AT_BASE
%token EMPTY_BRACKETS
%token A
%token EOF

%token LEFT_PAR RIGHT_PAR
%token LEFT_BRACKET RIGHT_BRACKET

%token <string> Uriref_
%token <string> Identifier
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
  directive DOT { Directive $1 }
| triples DOT { let (a,b) = $1 in Triples (a, b) }
;

directive:
  prefixID { $1 }
| base { $1 }
;

prefixID:
  AT_PREFIX n=option(Identifier) uri=uriref
    { Prefix (n, uri) }
base:
| AT_BASE uri=uriref
    { Base uri }
;

triples: subject predobjs { ($1, $2) }
;

subject:
| resource { Sub_res $1 }
| blank { Sub_blank $1 }
;

predobjs:
  separated_nonempty_list(SEMICOLON, predobj) option(SEMICOLON) { $1 }
;

predobj: verb separated_nonempty_list(COMMA, object_) { ($1, $2) }
;

verb:
| resource { Pred_res $1 }
| A { Pred_a }
;

resource:
| uriref { Uriref $1 }
| Qname_ { let (a, b) = $1 in Qname (a, b) }
| Identifier {
    let p = match $1 with "" -> None | s -> Some s in
    Qname (p, None)
  }
;

blank:
| Bname { NodeId $1 }
| EMPTY_BRACKETS { Empty }
| LEFT_BRACKET predobjs RIGHT_BRACKET { PredObjs $2 }
| collection { Collection $1 }
;

collection:
| LEFT_PAR list(object_) RIGHT_PAR { $2 }
;

object_:
| resource { Obj_res $1 }
| blank { Obj_blank $1 }
| literal { Obj_literal $1 }
;

literal:
| String_ lang=option(at_identifier) dt=option(datatype) { String ($1, lang, dt) }
| Integer { String ($1, None, Some (Uriref (Rdf_uri.string Rdf_rdf.xsd_integer))) }
| Decimal { String ($1, None, Some (Uriref (Rdf_uri.string Rdf_rdf.xsd_decimal))) }
| Double { String ($1, None, Some (Uriref (Rdf_uri.string Rdf_rdf.xsd_double))) }
| Boolean { String ($1, None, Some (Uriref (Rdf_uri.string Rdf_rdf.xsd_boolean))) }
;

at_identifier: AT Identifier { $2 }
;

datatype: HATHAT resource { $2 }
;

uriref: uri=Uriref_ { uri }
;

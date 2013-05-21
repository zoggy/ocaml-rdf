(** *)

%{
open Rdf_ttl_types

%}
%token SEMICOLON COMMA COLON DOT HATHAT AT
%token AT_PREFIX AT_BASE
%token EMPTY_BRACKETS
%token A

%token LEFT_PAR RIGHT_PAR
%token LEFT_BRACKET RIGHT_BRACKET

%token <string> Iriref
%token <string> Identifier
%token <string option * string option> Qname_
%token <string> Bname
%token <string> String_

%start <unit> main

%%

%public main : list(statement) { $1 }

statement:
  directive DOT { Directive $1 }
| triples DOT { let (a,b) = $1 in Triples (a, b) }
;

directive:
  prefixID { $1 }
| base { $1 }
;

prefixID:
  AT_PREFIX n=option(Identifier) COLON uri=uriref
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
;

at_identifier: AT Identifier { $2 }
;

datatype: HATHAT resource { $2 }
;

uriref: uri=Iriref { uri }
;

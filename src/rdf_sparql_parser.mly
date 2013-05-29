(** *)

%{
open Rdf_sparql_types;;

%}
%token <string>Iriref

%start <Rdf_sparql_types.query> query

%%

%public query:  { assert false }
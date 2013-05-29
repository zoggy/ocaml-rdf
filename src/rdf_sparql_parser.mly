(** *)

%{
open Rdf_sparql_types;;

let mk_loc start stop =
  { loc_start = start ;
    loc_end = stop ;
  }
;;
%}
%token <Rdf_sparql_types.iriref>Iriref_
%token <Rdf_sparql_types.var>Var_
%token <Rdf_sparql_types.prefixed_name> Pname_ln
%token <Rdf_sparql_types.pname_ns> Pname_ns

%token AS
%token BASE PREFIX
%token SELECT CONSTRUCT DESCRIBE ASK
%token DISTINCT REDUCED
%token VALUES FROM NAMED
%token STAR COLON

%start <Rdf_sparql_types.query> query

%%

%public query:
  p=prologue k=query_kind v=values_clause
  {
    { q_prolog = p ;
      q_kind = k ;
      q_values = v ;
    }
  }
;

prologue: list(prologue_item) { $1 };

prologue_item:
| BASE Iriref_ { BaseDecl $2 }
| PREFIX name=Pname_ns ir=Iriref_ { PrefixDecl (name, ir) }
;

query_kind:
| s=select_clause ds=list(dataset_clause) w=where_clause m=solution_modifier
  {
    Select {
      select_select = s ;
      select_dataset = ds ;
      select_where = w ;
      select_modifier = m ;
    }
  }
| CONSTRUCT
| DESCRIBE
| ASK { assert false }
;

select_clause:
| SELECT f=option(select_flag) v=select_vars
   {
    { sel_flag = f ;
      sel_vars = v ;
    }
  }
;

select_flag:
| DISTINCT { Distinct }
| REDUCED { Reduced }
;

select_vars:
| STAR { SelectAll }
| nonempty_list(select_var) { SelectVars $1 }
;

select_var:
| Var_ {
    { sel_var_loc = mk_loc $startpos($1) $endpos($1) ;
      sel_var_expr = None ;
      sel_var = $1 ;
    }
  }
| e=expression AS v=Var_ {
    { sel_var_loc = mk_loc $startpos(e) $endpos(v) ;
      sel_var_expr = Some e ;
      sel_var = v ;
    }
  }
;

dataset_clause:
| FROM source_selector { DefaultGraphClause $2 }
| FROM NAMED source_selector { NamedGraphClause $3 }
;

source_selector: iri { $1 }
;

iri:
| Iriref_ { Rdf_sparql_types.Iriref $1 }
| prefixed_name { Rdf_sparql_types.PrefixedName $1 }
;

prefixed_name:
| Pname_ns {
    { pname_loc = mk_loc $startpos($1) $endpos($1) ;
      pname_ns = $1 ;
      pname_local = None ;
    }
  }
| Pname_ln {
    $1
  }
;

values_clause:
| { None }
| VALUES d=datablock { Some d }
;

datablock:
| { assert false }
;

solution_modifier:
| { assert false }
;

where_clause:
| { assert false }
;

expression: { () };

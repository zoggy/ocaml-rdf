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
%token <int> Integer

%token AS
%token BASE PREFIX
%token SELECT CONSTRUCT DESCRIBE ASK
%token DISTINCT REDUCED
%token VALUES FROM NAMED GROUP BY HAVING ORDER ASC DESC LIMIT OFFSET
%token STAR COLON LPAR RPAR NIL COMMA

%start <Rdf_sparql_types.query> query

%%

%public query:
  p=prologue k=query_kind v=option(values_clause)
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
      select_select = s ; select_dataset = ds ;
      select_where = w ; select_modifier = m ;
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
| VALUES d=datablock { d }
;

datablock:
| { assert false }
;

solution_modifier:
| g=option(group_clause) h=option(having_clause) o=option(order_clause) lo=option(limit_offset_clause)
  {
    let loc = mk_loc $startpos(g) $endpos(lo) in
    { solmod_loc = loc ;
      solmod_group = (match g with None -> [] | Some l -> l) ;
      solmod_having = (match h with None -> [] | Some l -> l) ;
      solmod_order = o ;
      solmod_limoff = lo ;
    }
  }
;

group_clause:
| GROUP BY l=nonempty_list(group_condition)
  { l }
;

group_condition:
| builtin_call { GroupBuiltInCall $1 }
| function_call { GroupFunctionCall $1 }
| group_var { GroupVar $1 }
;

group_var:
| v=Var_ {
    let loc = mk_loc $startpos(v) $endpos(v) in
    { grpvar_loc = loc ; grpvar_expr = None ; grpvar = Some v }
  }
| e=expression AS v=Var_ {
    let loc = mk_loc $startpos(e) $endpos(v) in
    { grpvar_loc = loc ; grpvar_expr = Some e ; grpvar = Some v }
  }
| e=expression {
    let loc = mk_loc $startpos(e) $endpos(e) in
    { grpvar_loc = loc ; grpvar_expr = Some e ; grpvar = None }
  }
;

limit_offset_clause:
| l=limit_clause o=option(offset_clause)
  {
    let loc = mk_loc $startpos(l) $endpos(o) in
    { limoff_loc = loc ; limoff_limit = Some l ; limoff_offset = o }
  }
| o=offset_clause l=option(limit_clause)
  {
    let loc = mk_loc $startpos(o) $endpos(l) in
    { limoff_loc = loc ; limoff_limit = l ; limoff_offset = Some o }
  }
;

limit_clause: LIMIT Integer { $2 };
offset_clause: OFFSET Integer { $2 };

order_clause:
| ORDER BY l=nonempty_list(order_condition) { l }
;

order_condition:
| ASC bracketted_expression { OrderAsc $2 }
| DESC bracketted_expression { OrderDesc $2 }
| constraint_ { OrderConstr $1 }
| Var_ { OrderVar $1 }
;

having_clause:
| HAVING l=nonempty_list(having_condition) { l }
;

having_condition: constraint_ { $1 }
;

bracketted_expression: LPAR expression RPAR { $2 };

constraint_:
| bracketted_expression { ConstrExpr $1 }
| builtin_call { ConstrBuiltInCall $1 }
| function_call { ConstrFunctionCall $1 }
;

where_clause:
| { assert false }
;

builtin_call: Integer { () };

function_call:
| i=iri a=arg_list
  {
    let loc = mk_loc $startpos(i) $endpos(a) in
    { func_loc = loc ; func_iri = i ; func_args = a }
  }
;

arg_list:
| NIL {
    let loc = mk_loc $startpos($1) $endpos($1) in
    { argl_loc = loc ; argl_distinct = false ; argl = [] }
  }
| LPAR o=option(DISTINCT) l=separated_nonempty_list(COMMA, expression) RPAR
  {
    let loc = mk_loc $startpos($1) $endpos($4) in
    { argl_loc = loc ; argl_distinct = o <> None ; argl = l }
  }
;

expression: GROUP { () };

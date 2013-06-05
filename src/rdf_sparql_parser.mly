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
%token <string>Var1
%token <string>Var2
%token <Rdf_sparql_types.prefixed_name> Pname_ln
%token <Rdf_sparql_types.pname_ns> Pname_ns
%token <string> Blank_node_label
%token <int> Integer

%token A
%token ANON
%token AS
%token BASE PREFIX
%token SELECT CONSTRUCT DESCRIBE ASK
%token DISTINCT REDUCED
%token VALUES FROM NAMED GROUP BY HAVING ORDER ASC DESC LIMIT OFFSET WHERE
%token STAR COLON NIL COMMA DOT PIPE SLASH HAT BANG QM PLUS SEMICOLON
%token LPAR RPAR
%token LBRACE RBRACE
%token LBRA RBRA

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
| var {
    { sel_var_loc = mk_loc $startpos($1) $endpos($1) ;
      sel_var_expr = None ;
      sel_var = $1 ;
    }
  }
| e=expression AS v=var {
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
| LPAR { assert false }
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
| v=var {
    let loc = mk_loc $startpos(v) $endpos(v) in
    { grpvar_loc = loc ; grpvar_expr = None ; grpvar = Some v }
  }
| e=expression AS v=var {
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
| var { OrderVar $1 }
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
| WHERE group_graph_pattern { $2 }
;

group_graph_pattern:
| LBRACE subselect RBRACE { SubSelect $2 }
| LBRACE group_graph_pattern_sub RBRACE { GGPSub $2 }
;

subselect:
| sel=select_clause w=where_clause sol=solution_modifier v=values_clause
  {
   let loc = mk_loc $startpos(sel) $endpos(v) in
   {
    subsel_loc = loc ;
    subsel_select = sel ;
    subsel_where = w ;
    subsel_modifier = sol ;
    subsel_values = v ;
   }
  }
;

group_graph_pattern_sub:
| t=option(triples_block) l=list(gp_or_triples)
  {
    let loc = mk_loc $startpos(t) $endpos(l) in
    { ggp_sub_loc = loc ;
      ggp_sub_triples = t ;
      ggp_sub_rest = l;
    }
  }
;

gp_or_triples:
| gp=graph_pattern_not_triples option(DOT) t=option(triples_block)
  { (gp, t) }
;

triples_block:
| l=separated_nonempty_list(DOT,triples_same_subject_path)
  {
    let loc = mk_loc $startpos(l) $endpos(l) in
    { triples_loc = loc ;
      triples = l ;
    }
  }
;

triples_same_subject_path:
| v=var_or_term p=property_list_path_not_empty
  {
    let loc = mk_loc $startpos(v) $endpos(p) in
    let t =
      { tvtp_loc = loc ;
        tvtp_subject = v ;
        tvtp_path = p ;
      }
    in
    TriplesVar t
  }
| t=triples_node_path p=property_list_path
  {
    let loc = mk_loc $startpos(t) $endpos(p) in
    let t =
      {
        tnpp_loc = loc ;
        tnpp_path = t ;
        tnpp_props = p ;
      }
    in
    TriplesNodePath t
  }
;

property_list_path: l=option(property_list_path_not_empty) { l }
;

property_list_path_not_empty:
| v=verb_path_or_simple olp=object_list_path more=list(verbp_object_list_l)
  {
    let loc = mk_loc $startpos(v) $endpos(more) in
    let more = List.fold_left
      (fun acc -> function
         | None -> acc
         | Some x -> x :: acc
      )
      []
      more
    in
    {
      proplp_loc = loc ;
      proplp_verb = v ;
      proplp_objects = olp ;
      proplp_more = List.rev more ;
    }
  }
;

verbp_object_list_l:
| SEMICOLON option(verbp_object_list)
  { $2 }
;

verbp_object_list:
| v=verb_path_or_simple l=object_list
  {
    let loc = mk_loc $startpos(v) $endpos(l) in
    { propol_loc = loc ;
      propol_verb = v ;
      propol_objects = l
    }
  }
;

object_list: separated_nonempty_list(COMMA, object_) { $1 }
;

object_: graph_node { $1 }
;

verb_path_or_simple:
| path { VerbPath $1 }
| var { VerbSimple $1 }
;

graph_node:
| var_or_term { GraphNodeVT $1 }
| triples_node { GraphNodeTriples $1 }
;

triples_node:
| collection { TNodeCollection $1 }
| blank_node_property_list { TNodeBlank $1 }
;

collection:
LPAR nonempty_list(graph_node) RPAR { $2 }
;

blank_node_property_list:
LBRA property_list_not_empty RBRA { $2 }
;

property_list_not_empty:
| v=verb_object_list l=list(verb_object_list_l)
  {
    let l = List.fold_left
      (fun acc -> function
        | None -> acc
        | Some x -> x :: acc
       )
       []
       l
     in
     let l = List.rev l in
     v :: l
  }
;

verb_object_list_l:
| SEMICOLON option(verb_object_list)
  { $2 }
;

verb_object_list:
| v=verb l=object_list
  {
    let loc = mk_loc $startpos(v) $endpos(l) in
    { propol_loc = loc ;
      propol_verb = v ;
      propol_objects = l
    }
  }
;

verb:
| var_or_iri {
  match $1 with
    VIVar v -> VerbVar v
  | VIIri iri -> VerbIri iri
  }
| A { VerbA }
;

var_or_iri:
| var { VIVar $1 }
| iri { VIIri $1 }
;

var_or_term:
| var { Var $1 }
| graph_term { GraphTerm $1 }
;

var:
| s=Var1
  {
    let loc = mk_loc $startpos(s) $endpos(s) in
    { var_loc = loc ; var_name = s }
  }
| s=Var2
  {
    let loc = mk_loc $startpos(s) $endpos(s) in
    { var_loc = loc ; var_name = s }
  }
;

graph_term:
| iri { GraphTermIri $1 }
| rdf_literal { GraphTermLit $1 }
| numeric_literal { GraphTermNumeric $1 }
| boolean_literal { GraphTermBoolean $1 }
| blank_node { GraphTermBlank $1 }
| NIL { GraphTermNil }
;

graph_pattern_not_triples:
| LPAR { assert false }
;

object_list_path: separated_nonempty_list(COMMA, object_path) { $1 }
;

object_path: graph_node_path { $1 }
;

graph_node_path:
| var_or_term { GraphNodePathVT $1 }
| triples_node_path { GraphNodePathTriples $1 }
;

triples_node_path:
| collection_path { TNodePathCollection $1 }
| blank_node_property_list_path { TNodePathBlank $1 }
;

collection_path:
LPAR nonempty_list(graph_node_path) RPAR { $2 }
;

blank_node_property_list_path:
LBRA property_list_path_not_empty RBRA { $2 }
;

path: path_alternative { $1 };

path_alternative: separated_nonempty_list(PIPE, path_sequence)
  { $1 }
;

path_sequence: separated_nonempty_list(SLASH, path_elt_or_inverse)
  { $1 }
;

path_elt_or_inverse:
| path_elt { Elt $1 }
| HAT path_elt { Inv $2 }
;

path_elt:
| p=path_primary m=option(path_mod)
  {
    let loc = mk_loc $startpos(p) $endpos(m) in
    { pelt_loc = loc ;
      pelt_primary = p ;
      pelt_mod = m ;
    }
  }
;

path_primary:
| iri { PathIri $1 }
| A { PathA }
| BANG path_negated_property_list { PathNegPropSet $2 }
| LPAR path RPAR { Path $2 }
;

path_negated_property_list:
| path_one_in_property_set { [ $1 ] }
| LPAR l=separated_list(PIPE, path_one_in_property_set) { l }
;

path_one_in_property_set:
| iri { PathOneInIri $1 }
| A { PathOneInA }
| HAT iri { PathOneInNotIri $2 }
| HAT A { PathOneInNotA }
;

path_mod:
| QM { ModOptional }
| STAR { ModList }
| PLUS { ModOneOrMore }
;

blank_node:
| s=Blank_node_label
  {
    let loc = mk_loc $startpos(s) $endpos(s) in
    { bnode_loc = loc ;
      bnode_label = Some s ;
    }
  }
| ANON
  {
    let loc = mk_loc $startpos($1) $endpos($1) in
    { bnode_loc = loc ;
      bnode_label = None ;
    }
  }
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

numeric_literal:
| DOT { assert false }
;

boolean_literal:
| LPAR { assert false }
;

rdf_literal:
| RPAR { assert false }
;

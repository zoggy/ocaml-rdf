(** Print Sparql abstract syntax tree. *)

open Rdf_sparql_types

let map_opt f = function
  None -> None
| Some x -> Some (f x)
;;

let do_opt f = function
  None -> ()
| Some x -> f x
;;

let p = Buffer.add_string ;;
let pp = Printf.bprintf ;;

let print_list ?sep b f l =
  let rec iter = function
    [] -> ()
  | [path] -> f b path
  | path :: q ->
      f b path ;
      (match sep with None -> () | Some s -> p b s);
      iter q
  in
  iter l
;;

let print_iriref b ir = p b ("<" ^ (Rdf_uri.string ir.ir_iri) ^ ">")
let print_var b v = p b ("?"^v.var_name)
let print_bnode b bnode =
  match bnode.bnode_label with
    None -> p b "[]"
  | Some s -> p b ("_:"^s)

let print_path_mod b = function
| ModOptional -> p b "?"
| ModList -> p b "*"
| ModOneOrMore -> p b "+"
;;

let print_iri b = function
  | Iriref ir -> print_iriref b ir
  | PrefixedName pname ->
    p b (pname.pname_ns.pname_ns_name^":");
    (match pname.pname_local with
        None -> ()
      | Some l ->
         p b l.pname_local_name
    )
;;

let print_query_prolog_decl b = function
| BaseDecl iriref ->
   p b "BASE ";
   print_iriref b iriref;
   p b "\n"

| PrefixDecl (pname_ns, iriref) ->
   p b ("PREFIX "^pname_ns.pname_ns_name^": ");
   print_iriref b iriref;
   p b "\n"
;;

let print_query_prolog b decls =
  List.iter (print_query_prolog_decl b) decls
;;

let print_string_lit b s = pp b "%S" s

let print_rdf_literal b t =
  match t.rdf_lit.Rdf_node.lit_type with
    Some uri when Rdf_uri.equal uri Rdf_rdf.xsd_integer ->
      p b t.rdf_lit.Rdf_node.lit_value
  | _ ->
      let lit = t.rdf_lit in
      let lit, s_type =
        match t.rdf_lit_type with
          None -> lit, None
        | Some iri ->
            (
             { lit with Rdf_node.lit_type = None },
             Some (fun () -> p b "^^" ; print_iri b iri)
        )
      in
      p b (Rdf_node.string_of_node (Rdf_node.Literal lit));
      match s_type with
        None -> ()
      | Some f -> f ()
;;

let print_data_block_value b = function
  | DataBlockValueIri iri -> print_iri b iri
  | DataBlockValueRdf lit -> print_rdf_literal b lit
  | DataBlockValueNumeric lit -> print_rdf_literal b lit
  | DataBlockValueBoolean lit -> print_rdf_literal b lit
  | DataBlockValueUndef -> p b "UNDEF"
;;

let print_data_full_block_value b = function
  | Nil -> p b "()"
  | Value l ->
    p b "(";
    print_list ~sep: " " b print_data_block_value l ;
    p b ")";
;;

let print_inline_data_one_var b t =
  print_var b t.idov_var;
  p b " { ";
  print_list ~sep: " " b print_data_block_value t.idov_data;
  p b " } "
;;

let print_inline_data_full b t =
 p b "(";
 print_list ~sep: " " b print_var t.idf_vars ;
 p b ") { ";
 print_list ~sep: " " b print_data_full_block_value t.idf_values;
 p b " } "
;;

let print_datablock b = function
  | InLineDataOneVar i -> print_inline_data_one_var b i
  | InLineDataFull i -> print_inline_data_full b i

let print_values_clause b = function
  None -> ()
| Some d ->
    p b "\nVALUES " ;
    print_datablock b d
;;

let print_var_or_iri b = function
  | VIVar v -> print_var b v
  | VIIri iri -> print_iri b iri
;;


let rec print_select_var b t =
  match t.sel_var_expr with
    None -> print_var b t.sel_var
  | Some e ->
      p b "(" ;
      print_expression b e ;
      p b " AS ";
      print_var b t.sel_var;
      p b ")"

and print_select_vars b = function
  | SelectAll -> p b "*"
  | SelectVars l ->
    print_list ~sep: " " b print_select_var l

and print_select_clause b t =
  p b "SELECT ";
  (match t.sel_flag with
       None -> ()
     | Some Distinct -> p b "DISTINCT "
     | Some Reduced -> p b "REDUCED "
  );
  print_select_vars b t.sel_vars

and print_source_selector = print_iri

and print_dataset_clause b = function
  | DefaultGraphClause s ->
      p b "\nFROM ";
      print_source_selector b s
  | NamedGraphClause s ->
      p b "\nFROM NAMED ";
      print_source_selector b s

and print_arg_list b t  =
  p b "(";
  if t.argl_distinct then p b "DISTINCT ";
  print_list ~sep: ", " b print_expression t.argl ;
  p b ")"

and print_function_call b t =
  print_iri b t.func_iri ;
  print_arg_list b t.func_args

and string_of_bin_op = function
  | EEqual -> "="
  | ENotEqual -> "!="
  | ELt -> "<"
  | ELte -> "<="
  | EGt -> ">"
  | EGte -> ">="
  | EPlus -> "+"
  | EMinus -> "-"
  | EMult -> "*"
  | EDiv -> "/"
  | EOr -> "||"
  | EAnd -> "&&"

and print_expr b = function
  | EBin (e1, op, e2) ->
      print_expression b e1;
      p b (" "^(string_of_bin_op op)^" ");
      print_expression b e2
  | EIn (ne, l) ->
      print_expression b ne ;
      p b " IN (";
      print_list ~sep: ", " b print_expression l;
      p b ")"
  | ENotIn (ne, l) ->
      print_expression b ne;
      p b " NOT IN (";
      print_list ~sep: ", " b print_expression l;
      p b ")"

  | EUMinus e ->
      p b "- ";
      print_expression b e;
  | ENot e ->
      p b "!";
      print_expression b e

  | EBic c -> print_built_in_call b c
  | EFuncall c -> print_function_call b c
  | ELit lit -> print_rdf_literal b lit
  | ENumeric lit -> print_rdf_literal b lit
  | EBoolean lit -> print_rdf_literal b lit
  | EVar v -> print_var b v
  | EIri iri -> print_iri b iri

and print_expression b e =
  match e.expr with
    EVar _
  | EFuncall _
  | EBic _
  | ELit _
  | ENumeric _
  | EBoolean _  -> print_expr b e.expr
  | _ ->
      p b "(";
      print_expr b e.expr ;
      p b ")"

and print_aggregate b = function
  | Bic_COUNT (dis, eopt) ->
      pp b "COUNT(%s" (if dis then "DISTINCT " else "");
      (match eopt with
       | None -> p b "*"
       | Some e -> print_expression b e
      );
      p b ")"
  | Bic_SUM (dis, e) ->
      pp b "SUM(%s" (if dis then "DISTINCT " else "");
      print_expression b e;
      p b ")"
  | Bic_MIN (dis, e) ->
      pp b "MIN(%s" (if dis then "DISTINCT " else "");
      print_expression b e;
      p b ")"
  | Bic_MAX (dis, e) ->
      pp b "MAX(%s" (if dis then "DISTINCT " else "");
      print_expression b e;
      p b ")";
  | Bic_AVG (dis, e) ->
      pp b "AVG(%s" (if dis then "DISTINCT " else "");
      print_expression b e;
      p b ")"
  | Bic_SAMPLE (dis, e) ->
      pp b "SAMPLE(%s" (if dis then "DISTINCT " else "");
      print_expression b e;
      p b ")"
  | Bic_GROUP_CONCAT (dis, e, s_opt) ->
      pp b "GROUP_CONCAT(%s" (if dis then "DISTINCT " else "");
      print_expression b e ;
      (match s_opt with
        None -> ()
      | Some s -> p b "; SEPARATOR="; print_string_lit b s
      );
      p b ")"

and print_built_in_call b = function
  | Bic_agg agg -> print_aggregate b agg
  | Bic_fun (name, l) ->
      p b (name^"(");
      print_list ~sep: ", " b print_expression l;
      p b ")"
  | Bic_BOUND v ->
      p b "BOUND(";
      print_var b v ;
      p b ")"
  | Bic_EXISTS g ->
      p b "EXISTS";
      print_group_graph_pattern b g;
  | Bic_NOTEXISTS g ->
      p b "NOT EXISTS";
      print_group_graph_pattern b g

and print_group_condition b = function
  | GroupBuiltInCall c -> print_built_in_call b c
  | GroupFunctionCall c -> print_function_call b c
  | GroupVar gv ->
      match gv.grpvar_expr, gv.grpvar with
        None, None -> assert false
      | Some e, None ->
          p b "(";
          print_expression b e ;
          p b ")"
      | Some e, Some v ->
          p b "(" ;
          print_expression b e ;
          p b " AS ";
          print_var b v ;
          p b ")"
      | None, Some v ->
          print_var b v

and print_constraint b = function
  | ConstrBuiltInCall c -> print_built_in_call b c
  | ConstrFunctionCall c -> print_function_call b c
  | ConstrExpr e -> p b "("; print_expression b e ; p b ")"

and print_having_condition b t = print_constraint b t

and print_order_condition b = function
  | OrderAsc e ->
      p b "ASC (";
      print_expression b e;
      p b ")"
  | OrderDesc e ->
      p b "DESC (";
      print_expression b e;
      p b ")"
  | OrderConstr c -> print_constraint b c
  | OrderVar v -> print_var b v

and print_limit_offset_clause b t =
  (match t.limoff_offset with
    None -> ()
  | Some n -> pp b "OFFSET %d" n
  );
  (match t.limoff_limit with
    None -> ()
  | Some n -> pp b "LIMIT %d" n
  )

and print_solution_modifier b t =
  (match t.solmod_group with
     [] -> ()
   | l ->
       p b " GROUP BY ";
       print_list ~sep: " " b print_group_condition l
  );
  (match t.solmod_having with
     [] -> ()
   | l ->
      p b " HAVING " ;
      print_list ~sep: " " b print_having_condition l
  );
  (match t.solmod_order with
    None -> ()
  | Some l ->
      p b " ORDER BY " ;
      print_list ~sep: " " b print_order_condition l
  );
  do_opt (print_limit_offset_clause b) t.solmod_limoff

and print_bind b t =
  p b " BIND (";
  print_expression b t.bind_expr ;
  p b " AS ";
  print_var b t.bind_var ;
  p b ")"

and print_service_graph_pattern b t =
  p b " SERVICE ";
  if t.servgp_silent then p b "SILENT ";
  print_var_or_iri b t.servgp_name ;
  p b " ";
  print_group_graph_pattern b t.servgp_pat

and print_graph_graph_pattern b t =
  p b " GRAPH ";
  print_var_or_iri b t.graphgp_name ;
  print_group_graph_pattern b t.graphgp_pat

and print_graph_pattern_elt b = function
  | Triples l -> print_triples_block b l
  | Union l -> print_list ~sep: " UNION " b print_group_graph_pattern l
  | Optional g -> p b " OPTIONAL " ; print_group_graph_pattern b g
  | Minus g -> p b " MINUS "; print_group_graph_pattern b g
  | GGP g -> print_graph_graph_pattern b g
  | Service s -> print_service_graph_pattern b s
  | Filter c -> p b " FILTER "; print_constraint b c
  | Bind bind -> print_bind b bind
  | InlineData d -> p b " VALUES "; print_datablock b d

and print_graph_term b = function
  | GraphTermIri iri -> print_iri b iri
  | GraphTermLit lit -> print_rdf_literal b lit
  | GraphTermNumeric lit -> print_rdf_literal b lit
  | GraphTermBoolean lit -> print_rdf_literal b lit
  | GraphTermBlank bnode -> print_bnode b bnode
  | GraphTermNil -> p b "()"
  | GraphTermNode _ -> assert false

and print_var_or_term b = function
  | Var v -> print_var b v
  | GraphTerm t -> print_graph_term b t

and print_path_one_in_prop_set b = function
  | PathOneInIri iri -> print_iri b iri
  | PathOneInA -> p b "a"
  | PathOneInNotIri iri -> p b "^"; print_iri b iri
  | PathOneInNotA -> p b "^a"

and print_path_primary b = function
  | PathIri iri -> print_iri b iri
  | PathA -> p b "a"
  | PathNegPropSet l ->
      p b "!";
      (match l with
        [] -> assert false
      | [one] -> print_path_one_in_prop_set b one
      | l ->
        p b "( " ;
        print_list ~sep: " | " b print_path_one_in_prop_set l;
        p b " )";
      )
  | Path path ->
      p b "( "; print_path b path; p b " )";

and print_path_elt b t =
  print_path_primary b t.pelt_primary ;
  do_opt (print_path_mod b) t.pelt_mod

and print_path_elt_or_inverse b = function
  | Elt e -> print_path_elt b e
  | Inv e -> p b "^"; print_path_elt b e

and print_path_sequence b l =
  print_list ~sep: " / " b print_path_elt_or_inverse l

and print_path b l =
  print_list ~sep: " | " b print_path_sequence l

and print_verb b = function
  | VerbPath p -> print_path b p
  | VerbVar v -> print_var b v
  | VerbIri iri -> print_iri b iri
  | VerbA -> p b "a"

and print_triples_node b = function
  | TNodeCollection l ->
      p b "(";
      List.iter (print_graph_node b) l;
      p b ")"
  | TNodeBlank l ->
      p b "[ " ;
      List.iter (print_prop_object_list b) l ;
      p b " ]";

and print_graph_node b = function
  | GraphNodeVT t -> print_var_or_term b t
  | GraphNodeTriples t -> print_triples_node b t

and print_object b t = print_graph_node b t

and print_prop_object_list b t =
  print_verb b t.propol_verb ;
  p b " ";
  print_list ~sep: ";\n  " b print_object t.propol_objects

and print_triples_same_subject b = function
| TriplesVar (s, path) ->
      print_var_or_term b s;
      p b " " ;
      print_list ~sep:" ; " b print_prop_object_list path ;

| TriplesNode (t, path) ->
      print_triples_node b t;
      p b " " ;
      List.iter (print_prop_object_list b) path

and print_triples_block =
  let f b t =
    print_triples_same_subject b t;
    p b "."
  in
  fun b t -> List.iter (f b) t.triples

and print_ggp_sub b t =
  print_list ~sep: "\n" b print_graph_pattern_elt t.ggp_sub_elts

and print_group_graph_pattern b pat =
  p b "{\n" ;
   (match pat with
    | SubSelect s -> print_sub_select b s
    | GGPSub g -> print_ggp_sub b g
  );
  p b "\n}"

and print_sub_select b t =
  print_select_clause b t.subsel_select ;
  p b "\nWHERE ";
  print_group_graph_pattern b t.subsel_where ;
  p b "\n";
  print_solution_modifier b t.subsel_modifier ;
  print_values_clause b t.subsel_values
;;

let print_select_query b t =
  print_select_clause b t.select_select ;
  p b " " ;
  List.iter (print_dataset_clause b) t.select_dataset ;
  p b "\nWHERE " ;
  print_group_graph_pattern b t.select_where ;
  p b "\n";
  print_solution_modifier b t.select_modifier ;
;;

let print_triples_template b l =
  print_list ~sep: ".\n" b print_triples_same_subject l
let print_construct_template = print_triples_template

let print_construct_where b = function
  | Constr_ggp p -> print_group_graph_pattern b p
  | Constr_template t -> print_triples_template b t


let print_construct_query b t =
  p b "CONSTRUCT ";
  (
   match t.constr_template with
     Some tmpl ->
       print_construct_template b tmpl ;
       p b " ";
       List.iter (print_dataset_clause b) t.constr_dataset ;
       p b "\nWHERE ";
       print_construct_where b t.constr_where ;
   | None ->
       List.iter (print_dataset_clause b) t.constr_dataset ;
       p b "\nWHERE { ";
       print_construct_where b t.constr_where ;
       p b " }";

  );
  p b "\n";
  print_solution_modifier b t.constr_modifier

let print_describe_query b t =
  p b "DESCRIBE ";
  (match t.desc_sel with
    [] -> p b "*"
  | l -> print_list ~sep: " " b print_var_or_iri l;
  );
  p b " ";
  List.iter (print_dataset_clause b) t.desc_dataset ;
  (match t.desc_where with
     None -> ()
   | Some w ->
       p b "\nWHERE " ;
       print_group_graph_pattern b w
  );
  p b "\n";
  print_solution_modifier b t.desc_modifier
;;

let print_ask_query b t =
  p b "ASK ";
  List.iter (print_dataset_clause b) t.ask_dataset;
  p b "\nWHERE " ;
  print_group_graph_pattern b t.ask_where;
  p b "\n";
  print_solution_modifier b t.ask_modifier
;;

let print_query_kind b = function
  | Select q -> print_select_query b q
  | Construct q -> print_construct_query b q
  | Describe q -> print_describe_query b q
  | Ask q -> print_ask_query b q
;;


let print_query b q =
  print_query_prolog b q.q_prolog ;
  p b "\n";
  print_query_kind b q.q_kind ;
  p b "\n" ;
  print_values_clause b q.q_values
;;

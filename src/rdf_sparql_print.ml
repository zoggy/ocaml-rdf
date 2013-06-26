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

and print_relational_expression b = function
  | Numexp e -> print_numeric_expression b e
  | Equal (ne1, ne2) ->
      print_numeric_expression b ne1;
      p b " = ";
      print_numeric_expression b ne2
  | NotEqual (ne1, ne2) ->
      print_numeric_expression b ne1;
      p b " != ";
      print_numeric_expression b ne2
  | Lt (ne1, ne2) ->
      print_numeric_expression b ne1;
      p b " < ";
      print_numeric_expression b ne2
  | Gt (ne1, ne2) ->
      print_numeric_expression b ne1;
      p b " > ";
      print_numeric_expression b ne2
  | Lte (ne1, ne2) ->
      print_numeric_expression b ne1 ;
      p b " <= ";
      print_numeric_expression b ne2
  | Gte (ne1, ne2) ->
      print_numeric_expression b ne1 ;
      p b " >= ";
      print_numeric_expression b ne2
  | In (ne, l) ->
      print_numeric_expression b ne ;
      p b " IN (";
      print_list ~sep: ", " b print_expression l;
      p b ")"
| NotIn (ne, l) ->
      print_numeric_expression b ne;
      p b " NOT IN (";
      print_list ~sep: ", " b print_expression l;
      p b ")"

and print_numeric_expression b t = print_add_expression b t

and print_add_expression b (me, l) =
  print_mult_expression b me;
  List.iter (print_add_expression2 b) l

and print_add_expression2 b = function
| ExpPlus (me, l) ->
      p b " + ";
      print_mult_expression b me;
      List.iter (print_add_expression3 b) l

| ExpMinus (me, l) ->
      p b " - ";
      print_mult_expression b me;
      List.iter (print_add_expression3 b) l

| ExpPosNumeric (lit, l) ->
      print_rdf_literal b lit;
      List.iter (print_add_expression3 b) l

| ExpNegNumeric (lit, l) ->
      print_rdf_literal b lit;
      List.iter (print_add_expression3 b) l

and print_add_expression3 b = function
  | AddMult e ->
      p b " * ";
      print_unary_expression b e
  | AddDiv e ->
      p b " / ";
      print_unary_expression b e

and print_mult_expression b = function
  | Unary e -> print_unary_expression b e
  | Mult (ue, me) ->
      print_unary_expression b ue;
      p b " * ";
      print_mult_expression b me
  | Div (ue, me) ->
      print_unary_expression b ue;
      p b " / ";
      print_mult_expression b me

and print_unary_expression b = function
  | Primary e -> print_primary_expression b e
  | PrimNot e -> p b "!"; print_primary_expression b e
  | PrimPlus e -> p b "+"; print_primary_expression b e
  | PrimMinus e -> p b "-"; print_primary_expression b e

and print_primary_expression b = function
  | PrimExpr e ->
      p b "(";
      print_expression b e;
      p b ")"
  | PrimBuiltInCall c -> print_built_in_call b c
  | PrimFun c -> print_function_call b c
  | PrimLit lit -> print_rdf_literal b lit
  | PrimNumeric lit -> print_rdf_literal b lit
  | PrimBoolean lit -> print_rdf_literal b lit
  | PrimVar v -> print_var b v

and print_expression b t =
  print_list ~sep: "||" b print_and_expression t.expr_or_exprs

and print_and_expression b l =
  print_list ~sep: " && " b print_value_logical l
and print_value_logical b t = print_relational_expression b t

and print_built_in_call b = function
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
      p b ")";
  | Bic_STR e ->
      p b "STR(";
      print_expression b e;
      p b ")"
  | Bic_LANG e ->
      p b "LANG(";
      print_expression b e ;
      p b ")"
  | Bic_LANGMATCHES (e1, e2) ->
      p b "LANGMATCHES(";
      print_expression b e1;
      p b ", ";
      print_expression b e2;
      p b ")"
  | Bic_DATATYPE e ->
      p b "DATATYPE(";
      print_expression b e;
      p b ")"
  | Bic_BOUND v ->
      p b "BOUND(";
      print_var b v ;
      p b ")"
  | Bic_IRI e ->
      p b "IRI(";
      print_expression b e;
      p b ")"
  | Bic_URI e ->
      p b "URI(";
      print_expression b e;
      p b ")"
  | Bic_BNODE opt ->
      p b "BNODE(";
      do_opt (print_expression b) opt;
      p b ")"
  | Bic_RAND ->
      p b "RAND()"
  | Bic_ABS e ->
      p b "ABS(";
      print_expression b e;
      p b ")"
  | Bic_CEIL e ->
      p b "CEIL(";
      print_expression b e;
      p b ")"
  | Bic_FLOOR e ->
      p b "FLOOR(";
      print_expression b e;
      p b ")"
  | Bic_ROUND e ->
      p b "ROUND(";
      print_expression b e;
      p b ")"
  | Bic_CONCAT l ->
      p b "CONCAT(";
      print_list ~sep: ", " b print_expression l;
      p b ")"
  | Bic_SUBSTR (e1, e2, eopt) ->
      p b "SUBSTR(";
      print_expression b e1;
      p b ", ";
      print_expression b e2;
      (match eopt with
        None -> ()
      | Some e -> p b ", "; print_expression b e
      );
      p b ")"
  | Bic_STRLEN e ->
      p b "STRLEN(";
      print_expression b e;
      p b ")"
  | Bic_REPLACE (e1, e2, e3, eopt) ->
      p b "REPLACE(";
      print_expression b e1;
      p b ", ";
      print_expression b e2;
      p b ", ";
      print_expression b e3;
      (match eopt with
        None -> ()
      | Some e -> p b ", "; print_expression b e
      );
      p b ")"
  | Bic_UCASE e ->
      p b"UCASE(";
      print_expression b e;
      p b ")"
  | Bic_LCASE e ->
      p b "LCASE(";
      print_expression b e;
      p b ")"
  | Bic_ENCODE_FOR_URI e ->
      p b "ENCODE_FOR_URI(";
      print_expression b e;
      p b ")"
  | Bic_CONTAINS (e1, e2) ->
      p b "CONTAINS(";
      print_expression b e1;
      p b ", ";
      print_expression b e2;
      p b ")"
  | Bic_STRSTARTS (e1, e2) ->
      p b "STRSTARTS(";
      print_expression b e1;
      p b ", ";
      print_expression b e2;
      p b ")"
  | Bic_STRENDS (e1, e2) ->
      p b "STRENDS(";
      print_expression b e1;
      p b ", ";
      print_expression b e2;
      p b ")"
  | Bic_STRBEFORE (e1, e2) ->
      p b "STRBEFORE(";
      print_expression b e1;
      p b ", ";
      print_expression b e2;
      p b ")"
| Bic_STRAFTER (e1, e2) ->
      p b "STRAFTER(";
      print_expression b e1;
      p b ", ";
      print_expression b e2;
      p b ")"
  | Bic_YEAR e ->
      p b "YEAR(";
      print_expression b e;
      p b ")"
  | Bic_MONTH e ->
      p b "MONTH(";
      print_expression b e;
      p b ")"
  | Bic_DAY e ->
      p b "DAY(";
      print_expression b e;
      p b ")"
  | Bic_HOURS e ->
      p b "HOURS(";
      print_expression b e;
      p b ")"
  | Bic_MINUTES e ->
      p b "MINUTES(";
      print_expression b e;
      p b ")"
  | Bic_SECONDS e ->
      p b "SECONDS(";
      print_expression b e;
      p b ")"
  | Bic_TIMEZONE e ->
      p b "TIMEZONE(";
      print_expression b e;
      p b ")"
  | Bic_TZ e ->
      p b "TZ(";
      print_expression b e;
      p b ")"
  | Bic_NOW -> p b "NOW()"
  | Bic_UUID -> p b "UUID()"
  | Bic_STRUUID -> p b "STRUUID()"
  | Bic_MD5 e ->
      p b "MD5(";
      print_expression b e;
      p b ")"
  | Bic_SHA1 e ->
      p b "SHA1(";
      print_expression b e;
      p b ")"
  | Bic_SHA256 e ->
      p b "SHA256(";
      print_expression b e;
      p b ")"
  | Bic_SHA384 e ->
      p b "SHA384(";
      print_expression b e;
      p b ")"
  | Bic_SHA512 e ->
      p b "SHA512(";
      print_expression b e;
      p b ")"
  | Bic_COALESCE l ->
      p b "COALESCE(";
      print_list ~sep: ", " b print_expression l;
      p b ")"
  | Bic_IF (e1, e2, e3) ->
      p b "IF(";
      print_expression b e1;
      p b ", ";
      print_expression b e2;
      p b ", ";
      print_expression b e3;
      p b ")"
  | Bic_STRLANG (e1, e2) ->
      p b "STRLANG(";
      print_expression b e1;
      p b ", ";
      print_expression b e2;
      p b ")"
  | Bic_STRDT (e1, e2) ->
      p b "STRDT(";
      print_expression b e1;
      p b ", ";
      print_expression b e2;
      p b ")"
  | Bic_SAMETERM (e1, e2) ->
      p b "SAMETERM(";
      print_expression b e1;
      p b ", ";
      print_expression b e2;
      p b ")"
  | Bic_ISIRI e ->
      p b "ISIRI(";
      print_expression b e;
      p b ")"
  | Bic_ISURI e ->
      p b "ISURI(";
      print_expression b e;
      p b ")"
  | Bic_ISBLANK e ->
      p b "ISBLANK(";
      print_expression b e;
      p b ")"
  | Bic_ISLITERAL e ->
      p b "ISLITERAL(";
      print_expression b e;
      p b ")"
  | Bic_ISNUMERIC e ->
      p b "ISNUMERIC(";
      print_expression b e;
      p b ")"
  | Bic_REGEXP (e1, e2, eopt) ->
      p b "REGEXP(";
      print_expression b e1;
      p b ", ";
      print_expression b e2;
      (match eopt with
        None -> ()
      | Some e -> p b ", "; print_expression b e
      );
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

and print_graph_pattern_not_triples b = function
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

and print_verb_path b = function
  | VerbPath p -> print_path b p
  | VerbSimple v -> print_var b v

and print_verb b = function
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
      List.iter (print_verb_prop_object_list b) l ;
      p b " ]";

and print_graph_node b = function
  | GraphNodeVT t -> print_var_or_term b t
  | GraphNodeTriples t -> print_triples_node b t

and print_triples_node_path b = function
  | TNodePathCollection l ->
      p b "(" ;
      List.iter (print_graph_node_path b) l ;
      p b ")" ;
  | TNodePathBlank path ->
      p b "[ ";
      print_property_list_path b path;
      p b " ]";

and print_graph_node_path b = function
  | GraphNodePathVT t -> print_var_or_term b t
  | GraphNodePathTriples t -> print_triples_node_path b t

and print_object b t = print_graph_node b t
and print_object_path b t = print_graph_node_path b t

and print_prop_object_list :
  'a . (Buffer.t -> 'a -> unit) -> Buffer.t -> 'a prop_object_list -> unit =
      fun f b t ->
          f b t.propol_verb ;
          p b " ";
          print_list ~sep: ";\n  " b print_object t.propol_objects

and print_verb_path_prop_object_list b t =
  print_prop_object_list print_verb_path b t

and print_verb_prop_object_list b t =
  print_prop_object_list print_verb b t

and print_property_list_path b t =
    print_verb_path b t.proplp_verb ;
    p b " " ;
    List.iter (print_object_path b) t.proplp_objects ;
    let f m =
      p b " ;\n  ";
      print_verb_path_prop_object_list b m
    in
    List.iter f t.proplp_more

and print_triples_var_or_term_props :
  'a. (Buffer.t -> 'a -> unit) -> Buffer.t -> 'a triples_var_or_term_props -> unit =
  fun f b t ->
      print_var_or_term b t.tvtp_subject ;
      p b " " ;
      f b t.tvtp_path ;

and print_triples_node_path_props b t =
  print_triples_node_path b t.tnpp_path ;
  p b " " ;
  do_opt (print_property_list_path b) t.tnpp_props

and print_triples_same_subject_path b = function
  | TriplesPathVar t ->
      print_triples_var_or_term_props print_property_list_path b t
  | TriplesNodePath t ->
      print_triples_node_path_props b t

and print_triples_block =
  let f b t =
    print_triples_same_subject_path b t;
    p b "."
  in
  fun b t -> List.iter (f b) t.triples

and print_triples_node_props b t =
    print_triples_node b t.tnp_path ;
    p b " " ;
    List.iter (print_verb_prop_object_list b) t.tnp_props

and print_ggp_sub =
  let f_rest b (graph_pattern_not_triples, triples_block_option) =
        print_graph_pattern_not_triples b graph_pattern_not_triples ;
        p b " " ;
        do_opt (print_triples_block b) triples_block_option
  in
  fun b t ->
        do_opt (print_triples_block b) t.ggp_sub_triples ;
        p b " " ;
        List.iter (f_rest b) t.ggp_sub_rest

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


let print_triples_same_subject b = function
| TriplesVar t ->
    print_triples_var_or_term_props
      (fun b l -> print_list ~sep: " " b print_verb_prop_object_list l)
      b t;
| TriplesNode t -> print_triples_node_props b t

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

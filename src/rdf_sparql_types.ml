(** *)

type pos = Lexing.position ;;

type pname_ns = {
    pname_ns_pos : pos ;
    pname_ns_name : string option ;
  }
;;

type pname_local = {
    pname_local_pos : pos ;
    pname_local_name : string ;
  }

type var = {
  var_pos : pos ;
  var_name : string ;
  }

type iriref =
  { ir_loc : pos ;
    ir_iri : Rdf_uri.uri ;
  }

type prefixed_name =
  { pname_pos : pos ;
    pname_ns : pname_ns ;
    pname_local : pname_local option ;
  }

type iri =
  | Iriref of iriref
  | PrefixedName of prefixed_name
;;

type prefix_decl = pname_ns * iriref ;;

type query_prolog_decl =
  | BaseDecl of iriref
  | PrefixDecl of prefix_decl
;;

type query_prolog = query_prolog_decl list ;;

type rdf_literal =
  { rdf_lit_pos : pos ;
    rdf_lit : Rdf_node.literal ;
  }
;;

type data_block_value =
  | DataBlockValueIri of iri
  | DataBlockValueRdf of rdf_literal
  | DataBlockValueNumeric of rdf_literal
  | DataBlockValueBoolean of rdf_literal
  | DataBlockValueUndef
;;

type inline_data_one_var =
  { idov_pos : pos ;
    idov_var : var ;
    idov_data : data_block_value ;
  }

type datablock =
  | InLineDataOneVar
  | InLineDataFull

type values_clause = datablock option;;

type query_kind =
  | Select
  | Construct
  | Describe
  | Ask
;;


type query =
  { q_prolog : query_prolog ;
    q_kind : query_kind ;
    q_values : values_clause ;
  }
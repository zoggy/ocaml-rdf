module N = Rdf_term
val dbg : ?loc:string -> ?level:int -> (unit -> string) -> unit
type error =
    Unbound_variable of Rdf_sparql_types.var
  | Not_a_integer of Rdf_term.literal
  | Not_a_double_or_decimal of Rdf_term.literal
  | Type_mismatch of Rdf_dt.value * Rdf_dt.value
  | Invalid_fun_argument of Iri.t
  | Unknown_fun of Iri.t
  | Invalid_built_in_fun_argument of string *
      Rdf_sparql_types.expression list
  | Unknown_built_in_fun of string
  | No_term
  | Cannot_compare_for_datatype of Iri.t
  | Unhandled_regex_flag of char
  | Incompatible_string_literals of Rdf_dt.value * Rdf_dt.value
  | Empty_set of string
  | Missing_values_in_inline_data of Rdf_sparql_types.inline_data_full
  | Missing_implementation of string
exception Error of error
val error : error -> 'a
val string_of_error : error -> string
module Irimap = Iri.Map
module Iriset = Iri.Set
type context = {
  base : Iri.t;
  named : Iriset.t;
  dataset : Rdf_ds.dataset;
  active : Rdf_graph.graph;
  now : CalendarLib.Fcalendar.t;
}
val context :
  base:Iri.t ->
  ?from:Iri.t list -> ?from_named:Iriset.t -> Rdf_ds.dataset -> context
module GExprOrdered :
  sig
    type t = Rdf_term.term option list
    val compare :
      Rdf_term.term option list -> Rdf_term.term option list -> int
  end
module GExprMap :
  sig
    type key = GExprOrdered.t
    type 'a t = 'a Map.Make(GExprOrdered).t
    val empty : 'a t
    val is_empty : 'a t -> bool
    val mem : key -> 'a t -> bool
    val add : key -> 'a -> 'a t -> 'a t
    val singleton : key -> 'a -> 'a t
    val remove : key -> 'a t -> 'a t
    val merge :
      (key -> 'a option -> 'b option -> 'c option) -> 'a t -> 'b t -> 'c t
    val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
    val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
    val iter : (key -> 'a -> unit) -> 'a t -> unit
    val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val for_all : (key -> 'a -> bool) -> 'a t -> bool
    val exists : (key -> 'a -> bool) -> 'a t -> bool
    val filter : (key -> 'a -> bool) -> 'a t -> 'a t
    val partition : (key -> 'a -> bool) -> 'a t -> 'a t * 'a t
    val cardinal : 'a t -> int
    val bindings : 'a t -> (key * 'a) list
    val min_binding : 'a t -> key * 'a
    val max_binding : 'a t -> key * 'a
    val choose : 'a t -> key * 'a
    val split : key -> 'a t -> 'a t * 'a option * 'a t
    val find : key -> 'a t -> 'a
    val map : ('a -> 'b) -> 'a t -> 'b t
    val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
  end
val ebv : Rdf_dt.value -> bool
val compare : ?sameterm:bool -> Rdf_dt.value -> Rdf_dt.value -> int
val sortby_compare : Rdf_dt.value -> Rdf_dt.value -> int
val xsd_datetime : Iri.t
val fun_datetime : Rdf_dt.value list -> Rdf_dt.value
val iri_funs_ : (Iri.t * (Rdf_dt.value list -> Rdf_dt.value)) list
val iri_funs :
  (Rdf_sparql_ms.VMap.key list -> Rdf_sparql_ms.VMap.key) Irimap.t ref
val add_iri_fun :
  Irimap.key ->
  (Rdf_sparql_ms.VMap.key list -> Rdf_sparql_ms.VMap.key) -> unit
val bi_bnode :
  string ->
  ('a ->
   Rdf_sparql_ms.mu -> Rdf_sparql_types.expression -> Rdf_sparql_ms.VMap.key) ->
  'a -> Rdf_sparql_ms.mu -> Rdf_sparql_types.expression list -> Rdf_dt.value
val bi_coalesce :
  'a ->
  ('b -> 'c -> 'd -> Rdf_dt.value) -> 'b -> 'c -> 'd list -> Rdf_dt.value
val bi_datatype :
  string ->
  ('a -> 'b -> Rdf_sparql_types.expression -> Rdf_dt.value) ->
  'a -> 'b -> Rdf_sparql_types.expression list -> Rdf_dt.value
val bi_if :
  string ->
  ('a -> 'b -> Rdf_sparql_types.expression -> Rdf_dt.value) ->
  'a -> 'b -> Rdf_sparql_types.expression list -> Rdf_dt.value
val bi_iri :
  string ->
  (context -> 'a -> Rdf_sparql_types.expression -> Rdf_dt.value) ->
  context -> 'a -> Rdf_sparql_types.expression list -> Rdf_dt.value
val bi_uri :
  string ->
  (context -> 'a -> Rdf_sparql_types.expression -> Rdf_dt.value) ->
  context -> 'a -> Rdf_sparql_types.expression list -> Rdf_dt.value
val bi_isblank :
  string ->
  ('a -> 'b -> Rdf_sparql_types.expression -> Rdf_dt.value) ->
  'a -> 'b -> Rdf_sparql_types.expression list -> Rdf_dt.value
val bi_isiri :
  string ->
  ('a -> 'b -> Rdf_sparql_types.expression -> Rdf_dt.value) ->
  'a -> 'b -> Rdf_sparql_types.expression list -> Rdf_dt.value
val bi_isliteral :
  string ->
  ('a -> 'b -> Rdf_sparql_types.expression -> Rdf_dt.value) ->
  'a -> 'b -> Rdf_sparql_types.expression list -> Rdf_dt.value
val bi_lang :
  string ->
  ('a -> 'b -> Rdf_sparql_types.expression -> Rdf_dt.value) ->
  'a -> 'b -> Rdf_sparql_types.expression list -> Rdf_dt.value
val bi_isnumeric :
  string ->
  ('a -> 'b -> Rdf_sparql_types.expression -> Rdf_dt.value) ->
  'a -> 'b -> Rdf_sparql_types.expression list -> Rdf_dt.value
val regex_flag_of_char : char -> [> `CASELESS | `DOTALL | `MULTILINE ]
val bi_regex :
  string ->
  ('a -> 'b -> Rdf_sparql_types.expression -> Rdf_dt.value) ->
  'a -> 'b -> Rdf_sparql_types.expression list -> Rdf_dt.value
val bi_sameterm :
  string ->
  ('a -> 'b -> Rdf_sparql_types.expression -> Rdf_dt.value) ->
  'a -> 'b -> Rdf_sparql_types.expression list -> Rdf_dt.value
val bi_str :
  string ->
  ('a -> 'b -> Rdf_sparql_types.expression -> Rdf_dt.value) ->
  'a -> 'b -> Rdf_sparql_types.expression list -> Rdf_dt.value
val bi_strdt :
  string ->
  (context -> 'a -> Rdf_sparql_types.expression -> Rdf_dt.value) ->
  context -> 'a -> Rdf_sparql_types.expression list -> Rdf_dt.value
val bi_strlang :
  string ->
  ('a -> 'b -> Rdf_sparql_types.expression -> Rdf_dt.value) ->
  'a -> 'b -> Rdf_sparql_types.expression list -> Rdf_dt.value
val string_lit_compatible : 'a * 'b option -> 'c * 'b option -> bool
val bi_strlen :
  string ->
  ('a -> 'b -> Rdf_sparql_types.expression -> Rdf_dt.value) ->
  'a -> 'b -> Rdf_sparql_types.expression list -> Rdf_dt.value
val bi_substr :
  string ->
  ('a -> 'b -> Rdf_sparql_types.expression -> Rdf_dt.value) ->
  'a -> 'b -> Rdf_sparql_types.expression list -> Rdf_dt.value
val bi_strends :
  string ->
  ('a -> 'b -> Rdf_sparql_types.expression -> Rdf_dt.value) ->
  'a -> 'b -> Rdf_sparql_types.expression list -> Rdf_dt.value
val bi_strstarts :
  string ->
  ('a -> 'b -> Rdf_sparql_types.expression -> Rdf_dt.value) ->
  'a -> 'b -> Rdf_sparql_types.expression list -> Rdf_dt.value
val bi_contains :
  string ->
  ('a -> 'b -> Rdf_sparql_types.expression -> Rdf_dt.value) ->
  'a -> 'b -> Rdf_sparql_types.expression list -> Rdf_dt.value
val bi_strbefore :
  string ->
  ('a -> 'b -> Rdf_sparql_types.expression -> Rdf_dt.value) ->
  'a -> 'b -> Rdf_sparql_types.expression list -> Rdf_dt.value
val bi_strafter :
  string ->
  ('a -> 'b -> Rdf_sparql_types.expression -> Rdf_dt.value) ->
  'a -> 'b -> Rdf_sparql_types.expression list -> Rdf_dt.value
val bi_struuid :
  string ->
  'a -> 'b -> 'c -> Rdf_sparql_types.expression list -> Rdf_dt.value
val bi_uuid :
  string ->
  'a -> 'b -> 'c -> Rdf_sparql_types.expression list -> Rdf_dt.value
val bi_encode_for_uri :
  string ->
  ('a -> 'b -> Rdf_sparql_types.expression -> Rdf_dt.value) ->
  'a -> 'b -> Rdf_sparql_types.expression list -> Rdf_dt.value
val bi_concat :
  'a ->
  ('b -> 'c -> 'd -> Rdf_dt.value) -> 'b -> 'c -> 'd list -> Rdf_dt.value
val bi_langmatches :
  string ->
  ('a -> 'b -> Rdf_sparql_types.expression -> Rdf_dt.value) ->
  'a -> 'b -> Rdf_sparql_types.expression list -> Rdf_dt.value
val bi_replace :
  string ->
  ('a -> 'b -> Rdf_sparql_types.expression -> Rdf_dt.value) ->
  'a -> 'b -> Rdf_sparql_types.expression list -> Rdf_dt.value
val bi_numeric :
  (Rdf_dt.value -> Rdf_dt.value) ->
  string ->
  ('a -> 'b -> Rdf_sparql_types.expression -> Rdf_dt.value) ->
  'a -> 'b -> Rdf_sparql_types.expression list -> Rdf_dt.value
val bi_num_abs : Rdf_dt.value -> Rdf_dt.value
val bi_num_round : Rdf_dt.value -> Rdf_dt.value
val bi_num_ceil : Rdf_dt.value -> Rdf_dt.value
val bi_num_floor : Rdf_dt.value -> Rdf_dt.value
val bi_rand :
  string ->
  'a -> 'b -> 'c -> Rdf_sparql_types.expression list -> Rdf_dt.value
val bi_now :
  string ->
  'a -> context -> 'b -> Rdf_sparql_types.expression list -> Rdf_dt.value
val bi_on_date :
  (CalendarLib.Fcalendar.t -> Rdf_dt.value) ->
  string ->
  ('a -> 'b -> Rdf_sparql_types.expression -> Rdf_dt.value) ->
  'a -> 'b -> Rdf_sparql_types.expression list -> Rdf_dt.value
module C = CalendarLib.Fcalendar
val int_of_month : C.month -> int
val bi_date_year : C.t -> Rdf_dt.value
val bi_date_month : C.t -> Rdf_dt.value
val bi_date_day : C.t -> Rdf_dt.value
val bi_date_hours : C.t -> Rdf_dt.value
val bi_date_minutes : C.t -> Rdf_dt.value
val bi_date_seconds : C.t -> Rdf_dt.value
val bi_hash :
  (string -> Rdf_dt.value) ->
  string ->
  ('a -> 'b -> Rdf_sparql_types.expression -> Rdf_dt.value) ->
  'a -> 'b -> Rdf_sparql_types.expression list -> Rdf_dt.value
val bi_md5 : string -> Rdf_dt.value
val bi_sha1 : string -> Rdf_dt.value
val bi_sha256 : string -> Rdf_dt.value
val bi_lcase :
  string ->
  ('a -> 'b -> Rdf_sparql_types.expression -> Rdf_dt.value) ->
  'a -> 'b -> Rdf_sparql_types.expression list -> Rdf_dt.value
val bi_ucase :
  string ->
  ('a -> 'b -> Rdf_sparql_types.expression -> Rdf_dt.value) ->
  'a -> 'b -> Rdf_sparql_types.expression list -> Rdf_dt.value
val built_in_funs :
  ((context ->
    Rdf_sparql_ms.MuSet.elt ->
    Rdf_sparql_types.expression -> Rdf_sparql_ms.VMap.key) ->
   context ->
   Rdf_sparql_ms.MuSet.elt ->
   Rdf_sparql_types.expression list -> Rdf_sparql_ms.VMap.key)
  Rdf_sparql_types.SMap.t
val get_built_in_fun :
  string ->
  (context ->
   Rdf_sparql_ms.MuSet.elt ->
   Rdf_sparql_types.expression -> Rdf_sparql_ms.VMap.key) ->
  context ->
  Rdf_sparql_ms.MuSet.elt ->
  Rdf_sparql_types.expression list -> Rdf_sparql_ms.VMap.key
val eval_var : Rdf_sparql_ms.mu -> Rdf_sparql_types.var -> Rdf_dt.value
val eval_iri : Rdf_sparql_types.iri -> Rdf_dt.value
val eval_numeric2 :
  (int -> int -> int) ->
  (float -> float -> float) -> Rdf_dt.value * Rdf_dt.value -> Rdf_dt.value
val eval_plus :
  Rdf_sparql_ms.VMap.key * Rdf_sparql_ms.VMap.key -> Rdf_dt.value
val eval_minus :
  Rdf_sparql_ms.VMap.key * Rdf_sparql_ms.VMap.key -> Rdf_dt.value
val eval_mult :
  Rdf_sparql_ms.VMap.key * Rdf_sparql_ms.VMap.key -> Rdf_dt.value
val eval_div :
  Rdf_sparql_ms.VMap.key * Rdf_sparql_ms.VMap.key -> Rdf_dt.value
val eval_equal : Rdf_dt.value * Rdf_dt.value -> Rdf_dt.value
val eval_not_equal : Rdf_dt.value * Rdf_dt.value -> Rdf_dt.value
val eval_lt : Rdf_dt.value * Rdf_dt.value -> Rdf_dt.value
val eval_lte : Rdf_dt.value * Rdf_dt.value -> Rdf_dt.value
val eval_gt : Rdf_dt.value * Rdf_dt.value -> Rdf_dt.value
val eval_gte : Rdf_dt.value * Rdf_dt.value -> Rdf_dt.value
val eval_or : Rdf_dt.value * Rdf_dt.value -> Rdf_dt.value
val eval_and : Rdf_dt.value * Rdf_dt.value -> Rdf_dt.value
val eval_bin :
  Rdf_sparql_types.binary_op ->
  Rdf_sparql_ms.VMap.key * Rdf_sparql_ms.VMap.key -> Rdf_dt.value
val eval_expr :
  context ->
  Rdf_sparql_ms.MuSet.elt ->
  Rdf_sparql_types.expression -> Rdf_sparql_ms.VMap.key
val eval_bic :
  context ->
  Rdf_sparql_ms.MuSet.elt ->
  Rdf_sparql_types.built_in_call -> Rdf_sparql_ms.VMap.key
val eval_funcall :
  context ->
  Rdf_sparql_ms.MuSet.elt ->
  Rdf_sparql_types.function_call -> Rdf_sparql_ms.VMap.key
val eval_in :
  context ->
  Rdf_sparql_ms.MuSet.elt ->
  Rdf_sparql_types.expression ->
  Rdf_sparql_types.expression list -> Rdf_sparql_ms.VMap.key
val ebv_lit : Rdf_dt.value -> Rdf_term.literal
val eval_filter :
  context -> Rdf_sparql_ms.MuSet.elt -> Rdf_sparql_types.constraint_ -> bool
val filter_omega :
  context ->
  Rdf_sparql_types.constraint_ list ->
  Rdf_sparql_ms.Multimu.t -> Rdf_sparql_ms.Multimu.t
val join_omega :
  'a ->
  Rdf_sparql_ms.Multimu.t ->
  Rdf_sparql_ms.Multimu.t -> Rdf_sparql_ms.Multimu.t
val union_omega :
  Rdf_sparql_ms.Multimu.t ->
  Rdf_sparql_ms.Multimu.t -> Rdf_sparql_ms.Multimu.t
val leftjoin_omega :
  context ->
  Rdf_sparql_ms.Multimu.t ->
  Rdf_sparql_ms.Multimu.t ->
  Rdf_sparql_types.constraint_ list -> Rdf_sparql_ms.Multimu.t
val minus_omega :
  Rdf_sparql_ms.Multimu.t ->
  Rdf_sparql_ms.Multimu.t -> Rdf_sparql_ms.Multimu.t
val extend_omega :
  context ->
  Rdf_sparql_ms.Multimu.t ->
  Rdf_sparql_types.var ->
  Rdf_sparql_types.expression -> Rdf_sparql_ms.Multimu.t
val build_sort_comp_fun :
  Rdf_sparql_types.order_condition ->
  context -> Rdf_sparql_ms.MuSet.elt -> Rdf_sparql_ms.MuSet.elt -> int
val sort_solutions : 'a -> ('a -> 'b -> 'c -> int) list -> 'b -> 'c -> int
val sort_sequence :
  context ->
  Rdf_sparql_types.order_condition list ->
  Rdf_sparql_ms.MuSet.elt list -> Rdf_sparql_ms.MuSet.elt list
val project_sequence :
  Rdf_sparql_algebra.VS.t -> Rdf_sparql_ms.mu list -> Rdf_sparql_ms.mu list
val distinct : Rdf_sparql_ms.MuSet.elt list -> Rdf_sparql_ms.MuSet.elt list
val slice : 'a list -> int option -> int option -> 'a list
val group_omega :
  context ->
  Rdf_sparql_types.group_condition list ->
  Rdf_sparql_ms.Multimu.t -> Rdf_sparql_ms.Multimu.t GExprMap.t
val agg_count :
  context ->
  bool ->
  Rdf_sparql_ms.Multimu.t ->
  Rdf_sparql_types.expression option -> Rdf_dt.value
val agg_sum :
  context ->
  bool ->
  Rdf_sparql_ms.Multimu.t ->
  Rdf_sparql_types.expression -> Rdf_sparql_ms.VMap.key
val agg_fold :
  ('a -> Rdf_sparql_ms.VMap.key -> 'a) ->
  'a ->
  context ->
  bool -> Rdf_sparql_ms.Multimu.t -> Rdf_sparql_types.expression -> 'a
val agg_min :
  context ->
  bool ->
  Rdf_sparql_ms.Multimu.t ->
  Rdf_sparql_types.expression -> Rdf_sparql_ms.VMap.key
val agg_max :
  context ->
  bool ->
  Rdf_sparql_ms.Multimu.t ->
  Rdf_sparql_types.expression -> Rdf_sparql_ms.VMap.key
val agg_avg :
  context ->
  bool ->
  Rdf_sparql_ms.Multimu.t -> Rdf_sparql_types.expression -> Rdf_dt.value
val agg_sample : 'a -> 'b -> 'c -> 'd -> 'e
val agg_group_concat :
  context ->
  bool ->
  Rdf_sparql_ms.Multimu.t ->
  Rdf_sparql_types.expression -> string option -> Rdf_dt.value
val eval_agg :
  context ->
  Rdf_sparql_types.aggregate ->
  Rdf_sparql_ms.Multimu.t -> Rdf_sparql_ms.VMap.key
val aggregation :
  context ->
  Rdf_sparql_types.aggregate ->
  Rdf_sparql_ms.Multimu.t GExprMap.t -> Rdf_sparql_ms.VMap.key GExprMap.t
val aggregate_join :
  (context -> 'a -> Rdf_sparql_ms.Multimu.t) ->
  context ->
  Rdf_sparql_types.group_condition list * 'a ->
  Rdf_sparql_algebra.algebra list -> Rdf_sparql_ms.Multimu.t
val cons : 'a -> 'a list -> 'a list
val __print_mu : Rdf_sparql_ms.mu -> unit
val __print_omega : Rdf_sparql_ms.Multimu.t -> unit
val eval_datablock : Rdf_sparql_types.datablock -> Rdf_sparql_ms.Multimu.t
val eval : context -> Rdf_sparql_algebra.algebra -> Rdf_sparql_ms.Multimu.t
val eval_list :
  context -> Rdf_sparql_algebra.algebra -> Rdf_sparql_ms.MuSet.elt list

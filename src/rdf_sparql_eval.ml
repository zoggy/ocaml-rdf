(*********************************************************************************)
(*                OCaml-RDF                                                      *)
(*                                                                               *)
(*    Copyright (C) 2012-2013 Institut National de Recherche en Informatique     *)
(*    et en Automatique. All rights reserved.                                    *)
(*                                                                               *)
(*    This program is free software; you can redistribute it and/or modify       *)
(*    it under the terms of the GNU Lesser General Public License version        *)
(*    3 as published by the Free Software Foundation.                            *)
(*                                                                               *)
(*    This program is distributed in the hope that it will be useful,            *)
(*    but WITHOUT ANY WARRANTY; without even the implied warranty of             *)
(*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the              *)
(*    GNU General Public License for more details.                               *)
(*                                                                               *)
(*    You should have received a copy of the GNU General Public License          *)
(*    along with this program; if not, write to the Free Software                *)
(*    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA                   *)
(*    02111-1307  USA                                                            *)
(*                                                                               *)
(*    Contact: Maxence.Guesdon@inria.fr                                          *)
(*                                                                               *)
(*********************************************************************************)

(** *)

module N = Rdf_term
open Rdf_dt
open Rdf_sparql_types
open Rdf_sparql_algebra

let dbg = Rdf_misc.create_log_fun
  ~prefix: "Rdf_sparql_eval"
    "RDF_SPARQL_EVAL_DEBUG_LEVEL"
;;

let () = Random.self_init();;

type error =
| Unbound_variable of var
| Not_a_integer of Rdf_term.literal
| Not_a_double_or_decimal of Rdf_term.literal
| Type_mismatch of Rdf_dt.value * Rdf_dt.value
| Invalid_fun_argument of Rdf_iri.iri
| Unknown_fun of Rdf_iri.iri
| Invalid_built_in_fun_argument of string * expression list
| Unknown_built_in_fun of string
| No_term
| Cannot_compare_for_datatype of Rdf_iri.iri
| Unhandled_regex_flag of char
| Incompatible_string_literals of Rdf_dt.value * Rdf_dt.value
| Empty_set of string (** sparql function name *)
| Missing_values_in_inline_data of inline_data_full

exception Error of error
let error e = raise (Error e)

let string_of_error = function
| Unbound_variable v ->
    Printf.sprintf "%sUnbound variable %S"
      (Rdf_loc.string_of_loc v.var_loc) v.var_name
| Not_a_integer lit ->
    "Not an integer: "^(Rdf_term.string_of_literal lit)
| Not_a_double_or_decimal lit ->
    "Not an double: "^(Rdf_term.string_of_literal lit)
| Type_mismatch (v1, v2) ->
    "Type mismatch: "^(Rdf_dt.string_of_value v1)^" <!> "^(Rdf_dt.string_of_value v2)
| Invalid_fun_argument iri ->
    "Invalid argument for function "^(Rdf_iri.string iri)
| Unknown_fun iri ->
    "Unknown function "^(Rdf_iri.string iri)
| Invalid_built_in_fun_argument (name, _) ->
    "Invalid argument list for builtin function "^name
| Unknown_built_in_fun name ->
        "Unknown builtit function "^name
| No_term ->
    "No term"
| Cannot_compare_for_datatype iri ->
    "Cannot compare values of datatype "^(Rdf_iri.string iri)
| Unhandled_regex_flag c ->
    "Unhandled regexp flag "^(String.make 1 c)
| Incompatible_string_literals (v1, v2) -> (* FIXME: show values *)
    "Incompatible string literals"
| Empty_set name ->
    "Empty set in function "^name
| Missing_values_in_inline_data idf ->
    "Missing values in inline data"
;;

module Irimap = Rdf_iri.Irimap
module Iriset = Rdf_iri.Iriset

type context =
    { base : Rdf_iri.iri ;
      named : Iriset.t ;
      dataset : Rdf_ds.dataset ;
      active : Rdf_graph.graph ;
      now : Netdate.t ; (** because all calls to NOW() must return the same value,
        we get it at the beginning of the evaluation and use it when required *)
    }

let context ~base ?(from=[]) ?(from_named=Iriset.empty) dataset =
  let active =
    match from with
      [] when Iriset.is_empty from_named -> dataset.Rdf_ds.default
    | [] ->
        (* default graph is empty *)
        Rdf_graph.open_graph base
    | [iri] -> dataset.Rdf_ds.get_named iri
    | iris -> (* merge graphs to get the active graph *)
       let g = Rdf_graph.open_graph base in
       let graphs = List.map dataset.Rdf_ds.get_named iris in
       List.iter (Rdf_graph.merge g) graphs;
       g
  in
  let named =
    (* if no named graph is specified, then use the named graphs of
       dataset *)
    if Iriset.is_empty from_named then
      dataset.Rdf_ds.named
    else
      from_named
  in
  { base ; named = named ; dataset ; active ;
    now = Netdate.create (Unix.gettimeofday()) ;
  }
;;

module GExprOrdered =
  struct
    type t = Rdf_term.term option list
    let compare =
      let comp a b =
        match a, b with
          None, None -> 0
        | Some _, None -> 1
        | None, Some _ -> -1
        | Some a, Some b -> Rdf_term.compare a b
      in
      Rdf_misc.compare_list comp
  end
module GExprMap = Map.Make (GExprOrdered)

(** Evaluate boolean expression.
  See http://www.w3.org/TR/sparql11-query/#ebv *)
let ebv = function
  | Err e -> Rdf_dt.error e
  | Bool b -> b
  | String "" -> false
  | String _ -> true
  | Ltrl ("",_) -> false
  | Ltrl _ -> true
  | Ltrdt ("", _) -> false
  | Ltrdt _ -> true
  | Int n -> n <> 0
  | Float f ->
      begin
        match Pervasives.classify_float f with
          FP_nan | FP_zero -> false
        | _ -> true
      end
  | Datetime _
  | Rdf_dt.Iri _ | Rdf_dt.Blank _ -> false (* FIXME: or error ? *)
;;


let rec compare ?(sameterm=false) v1 v2 =
  (*prerr_endline
    ("compare v1="^(Rdf_dt.string_of_value v1)^" v2="^(Rdf_dt.string_of_value v2));*)
  match v1, v2 with
  | Err _, _ -> 1
  | _, Err _ -> -1
  | Rdf_dt.Iri t1, Rdf_dt.Iri t2 -> Rdf_iri.compare t1 t2
  | Rdf_dt.Blank s1, Rdf_dt.Blank s2 -> String.compare s1 s2
  | String s1, String s2
  | Ltrl (s1, None), String s2
  | String s1, Ltrl (s2, None) -> String.compare s1 s2
  | Int n1, Int n2 -> Pervasives.compare n1 n2
  | Int _, Float _ -> compare (Rdf_dt.float v1) v2
  | Float _, Int _ -> compare v1 (Rdf_dt.float v2)
  | Float f1, Float f2 -> Pervasives.compare f1 f2
  | Bool b1, Bool b2 -> Pervasives.compare b1 b2
  | Datetime t1, Datetime t2 ->
      Pervasives.compare (Netdate.since_epoch t1) (Netdate.since_epoch t2)
  | Ltrl (l1, lang1), Ltrl (l2, lang2) ->
      begin
        match Rdf_misc.opt_compare String.compare lang1 lang2 with
          0 -> String.compare l1 l2
        | n -> n
      end
  | Ltrdt (s1, dt1), Ltrdt (s2, dt2) ->
      (
       match Rdf_iri.compare dt1 dt2 with
         0 ->
           if sameterm then
             String.compare s1 s2
           else
             error (Cannot_compare_for_datatype dt1)
       | _ -> error (Type_mismatch (v1, v2))
      )
  | _, _ -> Rdf_dt.ValueOrdered.compare v1 v2
     (*error (Type_mismatch (v1, v2))*)

(** Implement the sorting order used in sparql order by clause:
  http://www.w3.org/TR/sparql11-query/#modOrderBy *)
let sortby_compare v1 v2 =
  try compare v1 v2
  with _ -> Rdf_dt.ValueOrdered.compare v1 v2
;;


(**  Predefined functions *)

let xsd_datetime = Rdf_rdf.xsd_ "dateTime";;
let fun_datetime = function
  [] | _::_::_ -> error (Invalid_fun_argument xsd_datetime)
| [v] -> Rdf_dt.datetime v

let iri_funs_ = [
    xsd_datetime, fun_datetime ;
  ];;

let iri_funs = ref (List.fold_left
  (fun acc (iri, f) -> Irimap.add iri f acc) Irimap.empty iri_funs_);;

let add_iri_fun iri f = iri_funs := Irimap.add iri f !iri_funs;;

(** Builtin functions; they take an expression evaluation function
  in parameter, as all arguments must not be always evaluated,
  for example in the IF.  *)


let bi_bnode name eval_expr ctx mu = function
  [] -> Blank (Rdf_sparql_ms.gen_blank_id())
| [e] ->
    begin
      let v = eval_expr ctx mu e in
      match v with
        String _
      | Ltrl (_, None) -> Rdf_sparql_ms.get_bnode mu v
      | _ -> Err (Rdf_dt.Type_error (v, "simple literal or string"))
    end
| l -> error (Invalid_built_in_fun_argument (name, l))
;;

let bi_coalesce _ =
  let rec iter eval_expr ctx mu = function
    [] -> error No_term
  | h :: q ->
    let v =
        try
          match eval_expr ctx mu h with
            Err _ -> None
          | v -> Some v
        with _ -> None
      in
      match v with
        None -> iter eval_expr ctx mu q
      | Some v -> v
  in
  iter
;;

let bi_datatype name =
  let f eval_expr ctx mu = function
    [e] -> Rdf_dt.datatype (eval_expr ctx mu e)
  | l -> error (Invalid_built_in_fun_argument (name, l))
  in
  f
;;

let bi_if name eval_expr ctx mu = function
  [e1 ; e2 ; e3] ->
    begin
       if ebv (eval_expr ctx mu e1) then
         eval_expr ctx mu e2
       else
         eval_expr ctx mu e3
    end
| l -> error (Invalid_built_in_fun_argument (name, l))
;;

let bi_iri name eval_expr ctx mu = function
  [e] -> Rdf_dt.iri ctx.base (eval_expr ctx mu e)
| l -> error (Invalid_built_in_fun_argument (name, l))
;;

let bi_uri name eval_expr ctx mu = function
  [e] -> Rdf_dt.iri ctx.base (eval_expr ctx mu e)
| l -> error (Invalid_built_in_fun_argument (name, l))
;;

let bi_isblank name =
  let f eval_expr ctx mu = function
    [e] ->
      (match eval_expr ctx mu e with
         Rdf_dt.Blank _ -> Bool true
       | _ -> Bool false
      )
  | l -> error (Invalid_built_in_fun_argument (name, l))
  in
  f
;;

let bi_isiri name =
  let f eval_expr ctx mu = function
    [e] ->
      (match eval_expr ctx mu e with
         Rdf_dt.Iri _ -> Bool true
       | _ -> Bool false
      )
  | l -> error (Invalid_built_in_fun_argument (name, l))
  in
  f
;;

let bi_isliteral name =
  let f eval_expr ctx mu = function
    [e] ->
      (match eval_expr ctx mu e with
         Rdf_dt.Blank _ | Rdf_dt.Iri _ | Rdf_dt.Err _ -> Bool false
       | Rdf_dt.String _ | Rdf_dt.Int _ | Rdf_dt.Float _ | Rdf_dt.Bool _
       | Rdf_dt.Datetime _ | Rdf_dt.Ltrl _ | Rdf_dt.Ltrdt _ ->
           Bool true
      )
  | l -> error (Invalid_built_in_fun_argument (name, l))
  in
  f
;;

let bi_lang name =
  let f eval_expr ctx mu = function
    [e] ->
      (match eval_expr ctx mu e with
        Ltrl (_, Some l) -> String l
      | _ -> String ""
      )
  | l -> error (Invalid_built_in_fun_argument (name, l))
  in
  f
;;

let bi_isnumeric name =
  let f eval_expr ctx mu = function
    [e] ->
      (match eval_expr ctx mu e with
       | Rdf_dt.Int _ | Rdf_dt.Float _ -> Bool true
       | Rdf_dt.Blank _ | Rdf_dt.Iri _ | Rdf_dt.Err _
       | Rdf_dt.String _ | Rdf_dt.Bool _
       | Rdf_dt.Datetime _ | Rdf_dt.Ltrl _ | Rdf_dt.Ltrdt _ ->
           Bool false
      )
  | l -> error (Invalid_built_in_fun_argument (name, l))
  in
  f
;;

let regex_flag_of_char = function
 | 's' -> `DOTALL
| 'm' -> `MULTILINE
| 'i' -> `CASELESS (* FIXME: 'x' not handled yet *)
| c -> error (Unhandled_regex_flag c)
;;

(** See http://www.w3.org/TR/xpath-functions/#regex-syntax *)
let bi_regex name =
  let flag_of_char r c = r := (regex_flag_of_char c) :: !r in
  let f eval_expr ctx mu l =
    let (s, pat, flags) =
      match l with
      | [e1 ; e2 ] -> (eval_expr ctx mu e1, eval_expr ctx mu e2, None)
      | [e1 ; e2 ; e3 ] ->
        (eval_expr ctx mu e1, eval_expr ctx mu e2,
         Some (eval_expr ctx mu e3))
      | _ -> error (Invalid_built_in_fun_argument (name, l))
    in
    try
      let (s, _) = Rdf_dt.string_literal s in
      let pat = match pat with
          String s -> s
        | _ -> Rdf_dt.error (Rdf_dt.Type_error (pat, "simple string"))
      in
      let flags =
        match flags with
          None -> []
        | Some (String s) ->
            let l = ref [] in
            String.iter (flag_of_char l) s;
            !l
        | Some v -> Rdf_dt.error (Rdf_dt.Type_error (v, "simple string"))
      in
      let flags = `UTF8 :: flags in
      dbg ~level: 2 (fun () -> name^": s="^s^" pat="^pat);
      let rex = Pcre.regexp ~flags pat in
      Bool (Pcre.pmatch ~rex s)
    with
      e ->
        dbg ~level: 1 (fun () -> name^": "^(Printexc.to_string e));
        Err (Rdf_dt.Exception e)
  in
  f
;;

let bi_sameterm name =
  let f eval_expr ctx mu = function
    [e1 ; e2] ->
      let v1 = eval_expr ctx mu e1 in
      let v2 = eval_expr ctx mu e2 in
      Bool (compare ~sameterm: true v1 v2 = 0)
  | l -> error (Invalid_built_in_fun_argument (name, l))
  in
  f
;;


let bi_str name =
  let f eval_expr ctx mu = function
    [e] ->
      (try Rdf_dt.string (eval_expr ctx mu e)
       with e -> Err (Rdf_dt.Exception e)
      )
  | l -> error (Invalid_built_in_fun_argument (name, l))
  in
  f
;;

let bi_strdt name =
  let f eval_expr ctx mu = function
    [e1 ; e2] ->
      (try
        let (s, _) = Rdf_dt.string_literal (eval_expr ctx mu e1) in
        let iri =
          match Rdf_dt.iri ctx.base (eval_expr ctx mu e2) with
            Rdf_dt.Iri t -> t
          | _ -> assert false
         in
         Ltrdt (s, iri)
       with e -> Err (Rdf_dt.Exception e)
      )
  | l -> error (Invalid_built_in_fun_argument (name, l))
  in
  f
;;

let bi_strlang name =
  let f eval_expr ctx mu = function
    [e1 ; e2] ->
      (try
        let (s, _) = Rdf_dt.string_literal (eval_expr ctx mu e1) in
        let (lang, _) = Rdf_dt.string_literal (eval_expr ctx mu e2) in
        Ltrl (s, Some lang)
       with e -> Err (Rdf_dt.Exception e)
      )
  | l -> error (Invalid_built_in_fun_argument (name, l))
  in
  f
;;

let string_lit_compatible lit1 lit2 =
  match lit1, lit2 with
    (_, Some x), (_, Some y) -> x = y
  | _ -> true;;

let bi_strlen name =
  let f eval_expr ctx mu = function
    [e] ->
      (try
         let (s, _) = Rdf_dt.string_literal (eval_expr ctx mu e) in
         Int (Rdf_utf8.utf8_string_length s)
       with e -> Err (Rdf_dt.Exception e)
      )
  | l -> error (Invalid_built_in_fun_argument (name, l))
  in
  f
;;

let bi_substr name =
  let f eval_expr ctx mu args =
    let (e, pos, len) =
      match args with
        [e1 ; e2 ] -> (e1, e2, None)
      | [e1 ; e2 ; e3] -> (e1, e2, Some e3)
      | _ -> error (Invalid_built_in_fun_argument (name, args))
    in
    try
      let (s, lang) = Rdf_dt.string_literal (eval_expr ctx mu e) in
      let pos =
        match Rdf_dt.int (eval_expr ctx mu pos) with
          Err e -> Rdf_dt.error e
        | Int n -> n
        | _ -> assert false
      in
      let len =
        match len with
          None -> None
        | Some e ->
            match eval_expr ctx mu e with
              Err e -> Rdf_dt.error e
            | Int n -> Some n
            | _ -> assert false
      in
      (* Convert positions to 0-based positions, and according
        to string length, since we return empty string in case of invalid bounds. *)
      let len_s = Rdf_utf8.utf8_string_length s in
      let start = pos - 1 in
      let len =
        match len with
          None -> len_s - start
        | Some len ->
            let len = start + len + 1 (* + 1 because we cremented start above *) in
            min (len_s - start) len
      in
      let start =
        if start < 0
        then 0
        else if start >= len_s then len_s - 1
          else start
      in
      let s = Rdf_utf8.utf8_substr s start len in
      Ltrl (s, lang)
    with e -> Err (Rdf_dt.Exception e)
  in
  f
;;

let bi_strends name =
  let f eval_expr ctx mu = function
    [e1 ; e2] ->
      (try
         let v1 = eval_expr ctx mu e1 in
         let v2 = eval_expr ctx mu e2 in
        let ((s1, lang1) as lit1) = Rdf_dt.string_literal v1 in
        let ((s2, lang2) as lit2) = Rdf_dt.string_literal v2 in
        if not (string_lit_compatible lit1 lit2) then
          error (Incompatible_string_literals (v1, v2));
        Bool (Rdf_utf8.utf8_is_suffix s1 s2)
       with e -> Err (Rdf_dt.Exception e)
      )
  | l -> error (Invalid_built_in_fun_argument (name, l))
  in
  f
;;

let bi_strstarts name =
  let f eval_expr ctx mu = function
    [e1 ; e2] ->
      (try
         let v1 = eval_expr ctx mu e1 in
         let v2 = eval_expr ctx mu e2 in
         let ((s1, lang1) as lit1) = Rdf_dt.string_literal v1 in
         let ((s2, lang2) as lit2) = Rdf_dt.string_literal v2 in
         if not (string_lit_compatible lit1 lit2) then
           error (Incompatible_string_literals (v1, v2));
         Bool (Rdf_utf8.utf8_is_prefix s1 s2)
       with e -> Err (Rdf_dt.Exception e)
      )
  | l -> error (Invalid_built_in_fun_argument (name, l))
  in
  f
;;

let bi_contains name =
  let f eval_expr ctx mu = function
    [e1 ; e2] ->
      (try
         let v1 = eval_expr ctx mu e1 in
         let v2 = eval_expr ctx mu e2 in
         let ((s1, lang1) as lit1) = Rdf_dt.string_literal v1 in
         let ((s2, lang2) as lit2) = Rdf_dt.string_literal v2 in
         if not (string_lit_compatible lit1 lit2) then
           error (Incompatible_string_literals (v1, v2));
         Bool (Rdf_utf8.utf8_contains s1 s2)
       with e -> Err (Rdf_dt.Exception e)
      )
  | l -> error (Invalid_built_in_fun_argument (name, l))
  in
  f
;;

let bi_strbefore name =
  let f eval_expr ctx mu = function
    [e1 ; e2] ->
      (try
         let v1 = eval_expr ctx mu e1 in
         let v2 = eval_expr ctx mu e2 in
         let ((s1, lang1) as lit1) = Rdf_dt.string_literal v1 in
         let ((s2, lang2) as lit2) = Rdf_dt.string_literal v2 in
         if not (string_lit_compatible lit1 lit2) then
           error (Incompatible_string_literals (v1, v2));
         String (Rdf_utf8.utf8_strbefore s1 s2)
       with e -> Err (Rdf_dt.Exception e)
      )
  | l -> error (Invalid_built_in_fun_argument (name, l))
  in
  f
;;
let bi_strafter name =
  let f eval_expr ctx mu = function
    [e1 ; e2] ->
      (try
         let v1 = eval_expr ctx mu e1 in
         let v2 = eval_expr ctx mu e2 in
         let ((s1, lang1) as lit1) = Rdf_dt.string_literal v1 in
         let ((s2, lang2) as lit2) = Rdf_dt.string_literal v2 in
         if not (string_lit_compatible lit1 lit2) then
           error (Incompatible_string_literals (v1, v2));
         String (Rdf_utf8.utf8_strafter s1 s2)
       with e -> Err (Rdf_dt.Exception e)
      )
  | l -> error (Invalid_built_in_fun_argument (name, l))
  in
  f
;;


let bi_struuid name =
  let f _ _ _ = function
    [] ->
      let uuid = Uuidm.create `V4 in
      String (Uuidm.to_string uuid)
  | l -> error (Invalid_built_in_fun_argument (name, l))
  in
  f
;;

let bi_uuid name =
  let f _ _ _ = function
    [] ->
      let uuid = Uuidm.create `V4 in
      let uuid = Uuidm.to_string uuid in
      Rdf_dt.Iri (Rdf_iri.iri ("urn:uuid:"^uuid))
  | l -> error (Invalid_built_in_fun_argument (name, l))
  in
  f
;;

let bi_encode_for_uri name =
  let f eval_expr ctx mu = function
    [e] ->
      (try
         let (s,_) = Rdf_dt.string_literal (eval_expr ctx mu e) in
         String (Netencoding.Url.encode ~plus: false s)
       with e -> Err (Rdf_dt.Exception e)
      )
  | l -> error (Invalid_built_in_fun_argument (name, l))
  in
  f
;;

let bi_concat name =
  let rec iter eval_expr ctx mu b lang = function
    [] when lang = None -> String (Buffer.contents b)
  | [] -> Ltrl (Buffer.contents b, lang)
  | e :: q ->
      let (s,lang2) as lit = Rdf_dt.string_literal (eval_expr ctx mu e) in
      let lang =
        match lang, lang2 with
          None, None -> None
        | None, Some _ -> lang2
        | Some _, None -> lang
        | Some x, Some y when x <> y ->
            error (Incompatible_string_literals
             (Ltrl (Buffer.contents b, lang), Ltrl (s,lang2)))
        | _ -> lang
      in
      Buffer.add_string b s ;
      iter eval_expr ctx mu b lang q
  in
  fun eval_expr ctx mu ->
    let b = Buffer.create 256 in
    iter eval_expr ctx mu b None
;;

let bi_langmatches name =
  let f eval_expr ctx mu = function
    [e1 ; e2] ->
      (try
         let v1 = eval_expr ctx mu e1 in
         let v2 = eval_expr ctx mu e2 in
         let ((s1, _) as lit1) = Rdf_dt.string_literal v1 in
         let ((s2, _) as lit2) = Rdf_dt.string_literal v2 in
         let b =
           match s2 with
             "*" -> s1 <> ""
           | _ ->
             (* by now, just check language spec s2 is a prefix of
               the given language tag s1 *)
             let s1 = String.lowercase s1 in
             let s2 = String.lowercase s2 in
             let len1 = String.length s1 in
             let len2 = String.length s2 in
               (len1 >= len2) &&
                 (String.sub s1 0 len2 = s2)
         in
         Bool b
       with e -> Err (Rdf_dt.Exception e)
      )
  | l -> error (Invalid_built_in_fun_argument (name, l))
  in
  f
;;

let bi_replace name =
  let flag_of_char r c = r := (regex_flag_of_char c) :: !r in
  let f eval_expr ctx mu l =
    let (s, pat, templ, flags) =
      match l with
      | [e1 ; e2 ; e3 ] ->
          (eval_expr ctx mu e1, eval_expr ctx mu e2, eval_expr ctx mu e3, None)
      | [e1 ; e2 ; e3 ; e4 ] ->
        (eval_expr ctx mu e1, eval_expr ctx mu e2, eval_expr ctx mu e3,
         Some (eval_expr ctx mu e4))
      | _ -> error (Invalid_built_in_fun_argument (name, l))
    in
    try
      let (s, _) = Rdf_dt.string_literal s in
      let pat = match pat with
          String s -> s
        | _ -> Rdf_dt.error (Rdf_dt.Type_error (pat, "simple string"))
      in
      let (templ, _) = Rdf_dt.string_literal templ in
      let flags =
        match flags with
          None -> []
        | Some (String s) ->
            let l = ref [] in
            String.iter (flag_of_char l) s;
            !l
        | Some v -> Rdf_dt.error (Rdf_dt.Type_error (v, "simple string"))
      in
      let flags = `UTF8 :: flags in
      dbg ~level: 2 (fun () -> name^": s="^s^" pat="^pat^" templ="^templ);
      let rex = Pcre.regexp ~flags pat in
      String (Pcre.replace ~rex ~templ s)
    with
      e ->
        dbg ~level: 1 (fun () -> name^": "^(Printexc.to_string e));
        Err (Rdf_dt.Exception e)
  in
  f
;;

let bi_numeric f name =
  let f eval_expr ctx mu = function
    [e] ->
      let v =
        try Rdf_dt.numeric (eval_expr ctx mu e)
        with e -> Err (Rdf_dt.Exception e)
      in
      (
       match v with
         Err e -> Err e
       | _ -> f v
      )
  | l -> error (Invalid_built_in_fun_argument (name, l))
  in
  f
;;

let bi_num_abs = function
  Int n -> Int (abs n)
| Float f -> Float (abs_float f)
| _ -> assert false
;;


let bi_num_round = function
  Int n -> Int n
| Float f ->  Float (Pervasives.float (int_of_float (floor (f +. 0.5))))
| _ -> assert false
;;


let bi_num_ceil = function
  Int n -> Int n
| Float f -> Float (ceil f)
| _ -> assert false
;;


let bi_num_floor = function
  Int n -> Int n
| Float f -> Float (floor f)
| _ -> assert false
;;

let bi_rand name _ _ _ = function
  [] -> Float (Random.float 1.0)
| l -> error (Invalid_built_in_fun_argument (name, l))
;;

let bi_now name _ ctx _ = function
  [] -> Datetime ctx.now
| l -> error (Invalid_built_in_fun_argument (name, l))
;;

let bi_on_date f name =
  let f eval_expr ctx mu = function
    [e] ->
      let v =
        try Rdf_dt.datetime (eval_expr ctx mu e)
        with e -> Err (Rdf_dt.Exception e)
      in
      (
       match v with
         Err e -> Err e
       | Datetime t -> f t
       | _ -> assert false
      )
  | l -> error (Invalid_built_in_fun_argument (name, l))
  in
  f
;;

let bi_date_year t = Int t.Netdate.year ;;
let bi_date_month t = Int t.Netdate.month ;;
let bi_date_day t = Int t.Netdate.day ;;
let bi_date_hours t = Int t.Netdate.hour ;;
let bi_date_minutes t = Int t.Netdate.minute ;;
let bi_date_seconds t =
  let dec = (float_of_int t.Netdate.nanos) /. 1_000_000_000.0 in
  Float (float_of_int t.Netdate.second +. dec)
;;

let bi_hash f name =
  let f eval_expr ctx mu = function
    [e] ->
      let v =
        try Rdf_dt. (eval_expr ctx mu e)
        with e -> Err (Rdf_dt.Exception e)
      in
      (
       match v with
         Err e -> Err e
       | String s -> f s
       | _ -> Rdf_dt.error (Rdf_dt.Type_error (v, "simple string"))
      )
  | l -> error (Invalid_built_in_fun_argument (name, l))
  in
  f;;

let bi_md5 s = String (String.lowercase (Digest.to_hex (Digest.string s)));;
let bi_sha1 s =
  let hash = Cryptokit.Hash.sha1 () in
  hash#add_string s ;
  let t = Cryptokit.Hexa.encode () in
  t#put_string hash#result ;
  String (String.lowercase t#get_string)
;;
let bi_sha256 s =
  let hash = Cryptokit.Hash.sha256 () in
  hash#add_string s ;
  let t = Cryptokit.Hexa.encode () in
  t#put_string hash#result ;
  String (String.lowercase t#get_string)
;;

let bi_lcase name =
  let f eval_expr ctx mu = function
    [e] ->
      (try
         let (s,_) = Rdf_dt.string_literal (eval_expr ctx mu e) in
         String (Rdf_utf8.utf8_lowercase s)
       with e -> Err (Rdf_dt.Exception e)
      )
  | l -> error (Invalid_built_in_fun_argument (name, l))
  in
  f
;;

let bi_ucase name =
  let f eval_expr ctx mu = function
    [e] ->
      (try
         let (s,_) = Rdf_dt.string_literal (eval_expr ctx mu e) in
         String (Rdf_utf8.utf8_uppercase s)
       with e -> Err (Rdf_dt.Exception e)
      )
  | l -> error (Invalid_built_in_fun_argument (name, l))
  in
  f
;;

let built_in_funs =
  let l =
    [
      "ABS", bi_numeric bi_num_abs ;
      "BNODE", bi_bnode ;
      "CEIL", bi_numeric bi_num_ceil ;
      "COALESCE", bi_coalesce ;
      "CONCAT", bi_concat ;
      "CONTAINS", bi_contains ;
      "DATATYPE", bi_datatype ;
      "DAY", bi_on_date bi_date_day ;
      "ENCODE_FOR_URI", bi_encode_for_uri ;
      "FLOOR", bi_numeric bi_num_floor ;
      "HOURS", bi_on_date bi_date_hours ;
      "IF", bi_if ;
      "ISBLANK", bi_isblank ;
      "IRI", bi_iri ;
      "ISIRI", bi_isiri ;
      "ISLITERAL", bi_isliteral ;
      "ISNUMERIC", bi_isnumeric ;
      "ISURI", bi_isiri ;
      "LANG", bi_lang ;
      "LANGMATCHES", bi_langmatches ;
      "LCASE", bi_lcase ;
      "MD5", bi_hash bi_md5 ;
      "MINUTES", bi_on_date bi_date_minutes ;
      "MONTH", bi_on_date bi_date_month ;
      "NOW", bi_now ;
      "RAND", bi_rand ;
      "REGEX", bi_regex ;
      "REPLACE", bi_replace ;
      "ROUND", bi_numeric bi_num_round ;
      "SAMETERM", bi_sameterm ;
      "SECONDS", bi_on_date bi_date_seconds ;
      "SHA1", bi_hash bi_sha1 ;
      "SHA256", bi_hash bi_sha256 ;
      "STR", bi_str ;
      "STRAFTER", bi_strafter ;
      "STRBEFORE", bi_strbefore ;
      "STRDT", bi_strdt ;
      "STRENDS", bi_strends ;
      "STRLANG", bi_strlang ;
      "STRLEN", bi_strlen ;
      "STRSTARTS", bi_strstarts ;
      "STRUUID", bi_struuid ;
      "SUBSTR", bi_substr ;
      "UCASE", bi_ucase ;
      "URI", bi_iri ;
      "UUID", bi_uuid ;
      "YEAR", bi_on_date bi_date_year ;
    ]
  in
  List.fold_left
    (fun acc (name, f) -> SMap.add name (f name) acc)
    SMap.empty l
;;



let get_built_in_fun name =
  let name = String.uppercase name in
  try SMap.find name built_in_funs
  with Not_found -> error (Unknown_built_in_fun name)
;;

let eval_var mu v =
  try
    let term = Rdf_sparql_ms.mu_find_var v mu in
    Rdf_dt.of_term term
  with Not_found -> error (Unbound_variable v)
;;

let eval_iri = function
  Iriref ir -> Rdf_dt.Iri ir.ir_iri
| PrefixedName _ | Reliri _-> assert false
;;

let rec eval_numeric2 f_int f_float (v1, v2) =
 try
   match (v1, v2) with
    | (Float f1, Float f2) -> Float (f_float f1 f2)
    | (Int n1, Int n2) -> Int (f_int n1 n2)
    | ((Float _) as v1, ((Int _) as v2)) ->
        eval_numeric2 f_int f_float (v1, Rdf_dt.float v2)
    | ((Int _) as v1, ((Float _) as v2)) ->
        eval_numeric2 f_int f_float (Rdf_dt.float v1, v2)
    | v1, v2 ->
        eval_numeric2 f_int f_float
          ((Rdf_dt.numeric v1), (Rdf_dt.numeric v2))
  with
    e -> Err (Rdf_dt.Exception e)
;;

let eval_plus = eval_numeric2 (+) (+.)
let eval_minus = eval_numeric2 (-) (-.)
let eval_mult = eval_numeric2 ( * ) ( *. )
let eval_div = eval_numeric2 (/) (/.)

let eval_equal (v1, v2) = Bool (compare v1 v2 = 0)
let eval_not_equal (v1, v2) = Bool (compare v1 v2 <> 0)
let eval_lt (v1, v2) = Bool (compare v1 v2 < 0)
let eval_lte (v1, v2) = Bool (compare v1 v2 <= 0)
let eval_gt (v1, v2) = Bool (compare v1 v2 > 0)
let eval_gte (v1, v2) = Bool (compare v1 v2 >= 0)

let eval_or = function
  (Err e, Err _) -> Err e
| (Err e, v)
| (v, Err e) ->
    if ebv v then Bool true else Err e
| v1, v2 -> Bool ((ebv v1) || (ebv v2))

let eval_and = function
  (Err e, Err _) -> Err e
| (Err e, v)
| (v, Err e) ->
    if ebv v then Err e else Bool false
| v1, v2 -> Bool ((ebv v1) && (ebv v2))

let eval_bin = function
| EPlus -> eval_plus
| EMinus -> eval_minus
| EMult -> eval_mult
| EDiv -> eval_div
| EEqual -> eval_equal
| ENotEqual -> eval_not_equal
| ELt -> eval_lt
| EGt -> eval_gt
| ELte -> eval_lte
| EGte -> eval_gte
| EOr -> eval_or
| EAnd -> eval_and

let rec eval_expr : context -> Rdf_sparql_ms.mu -> expression -> Rdf_dt.value =
  fun ctx mu e ->
    match e.expr with
      EVar v -> eval_var mu v
    | EIri iri -> eval_iri iri
    | EBin (e1, op, e2) ->
        let v1 = eval_expr ctx mu e1 in
        let v2 = eval_expr ctx mu e2 in
        eval_bin op (v1, v2)
    | ENot e ->
        let b = ebv (eval_expr ctx mu e) in
        Bool (not b)
    | EUMinus e ->
        let v = eval_expr ctx mu e in
        eval_bin EMinus (Int 0, v)
    | EBic c -> eval_bic ctx mu c
    | EFuncall c -> eval_funcall ctx mu c
    | ELit lit
    | ENumeric lit
    | EBoolean lit -> Rdf_dt.of_literal lit.rdf_lit
    | EIn (e, l) -> eval_in ctx mu e l
    | ENotIn (e, l) ->
        match eval_in ctx mu e l with
          Bool b -> Bool (not b)
        | Err e -> Err e
        | _ -> assert false

and eval_bic ctx mu = function
  | Bic_agg agg -> assert false
  | Bic_fun (name, args) ->
      let f = get_built_in_fun name in
      f eval_expr ctx mu args
  | Bic_BOUND v ->
      (try ignore(Rdf_sparql_ms.mu_find_var v mu); Bool true
       with _ -> Bool false)
  | Bic_EXISTS _
  | Bic_NOTEXISTS _ -> assert false
     (* FIXME: need to translate this in algebra, with type parameter for expressions ... ? *)

and eval_funcall ctx mu c =
  let f =
    let iri =
      match c.func_iri with
        Iriref ir -> ir.ir_iri
      | _ -> assert false
    in
    try Irimap.find iri !iri_funs
    with Not_found -> error (Unknown_fun iri)
  in
  let args = List.map (eval_expr ctx mu) c.func_args.argl in
  f args

and eval_in =
  let eval eval_expr ctx mu v0 e acc =
    let v = eval_expr ctx mu e in
    let b =
      try Bool (compare v0 v = 0)
      with e -> Err (Rdf_dt.Exception e)
    in
    eval_or (b, acc)
  in
  fun ctx mu e0 l ->
    match l with
      [] -> Bool false
    | _ ->
      let v0 = eval_expr ctx mu e0 in
      List.fold_right (eval eval_expr ctx mu v0) l (Bool false)

and ebv_lit v = Rdf_term.mk_literal_bool (ebv v)

let eval_filter ctx mu c =
  let e =
    match c with
      ConstrBuiltInCall c ->
        { expr_loc = Rdf_loc.dummy_loc ; expr = EBic c }
    | ConstrFunctionCall c ->
        { expr_loc = Rdf_loc.dummy_loc ; expr = EFuncall c }
    | ConstrExpr e -> e
  in
  ebv (eval_expr ctx mu e)


let filter_omega =
  let pred ctx filters mu = List.for_all (eval_filter ctx mu) filters in
  fun ctx filters o -> Rdf_sparql_ms.omega_filter (pred ctx filters) o

let join_omega ctx o1 o2 =
  Rdf_sparql_ms.omega_join o1 o2

let union_omega o1 o2 = Rdf_sparql_ms.omega_union o1 o2

let leftjoin_omega =
  let pred ctx filters mu = List.for_all (eval_filter ctx mu) filters in
  fun ctx o1 o2 filters ->
    let pred = pred ctx filters in
    let filter_part = Rdf_sparql_ms.omega_join ~pred o1 o2 in
    let diff_part = Rdf_sparql_ms.omega_diff_pred pred o1 o2 in
    union_omega filter_part diff_part

let minus_omega o1 o2 = Rdf_sparql_ms.omega_minus o1 o2

let extend_omega ctx o var expr =
  let eval mu = Rdf_dt.to_term (eval_expr ctx mu expr) in
  Rdf_sparql_ms.omega_extend eval o var


let rec build_sort_comp_fun = function
| OrderAsc e ->
    begin
      fun ctx mu1 mu2 ->
        let v1 = eval_expr ctx mu1 e in
        let v2 = eval_expr ctx mu2 e in
        sortby_compare v1 v2
    end
| OrderDesc e ->
    begin
      fun ctx mu1 mu2 ->
        let v1 = eval_expr ctx mu1 e in
        let v2 = eval_expr ctx mu2 e in
        sortby_compare v2 v1
    end
| OrderVar v ->
    begin
      fun ctx mu1 mu2 ->
        let v1 =
          try Rdf_dt.of_term (Rdf_sparql_ms.mu_find_var v mu1)
          with e -> Rdf_dt.Err (Rdf_dt.Exception e)
        in
        let v2 =
          try Rdf_dt.of_term (Rdf_sparql_ms.mu_find_var v mu2)
          with e -> Rdf_dt.Err (Rdf_dt.Exception e)
        in
        sortby_compare v1 v2
    end
| OrderConstr t ->
    match t with
      (ConstrExpr e) ->
        build_sort_comp_fun (OrderAsc e)
    | (ConstrBuiltInCall bic) ->
        let e = { expr_loc = Rdf_loc.dummy_loc ; expr = EBic bic } in
        build_sort_comp_fun (OrderAsc e)
    | (ConstrFunctionCall fc) ->
        let e = { expr_loc = Rdf_loc.dummy_loc ; expr = EFuncall fc } in
        build_sort_comp_fun (OrderAsc e)
;;

let sort_solutions =
  let rec sort ctx mu1 mu2 = function
    [] -> 0
  | f :: q ->
      match f ctx mu1 mu2 with
        0 -> sort ctx mu1 mu2 q
      | n -> n
  in
  fun ctx comp_funs mu1 mu2 -> sort ctx mu1 mu2 comp_funs
;;

let sort_sequence ctx order_conds solutions =
  (*prerr_endline
    (Printf.sprintf "sort_sequence: %d solutions, %d order_conds"
      (List.length solutions) (List.length order_conds));
   *)
  let comp_funs = List.map build_sort_comp_fun order_conds in
  let compare = sort_solutions ctx comp_funs in
  List.sort compare solutions
;;

let project_sequence vars l =
  let vars = Rdf_sparql_algebra.VS.fold
    (fun v acc -> Rdf_sparql_types.SSet.add v.var_name acc)
      vars Rdf_sparql_types.SSet.empty
  in
  List.map (Rdf_sparql_ms.mu_project vars) l

let distinct =
  let f (set, acc) mu =
    if Rdf_sparql_ms.MuSet.mem mu set then
      (set, acc)
    else
      (Rdf_sparql_ms.MuSet.add mu set, mu :: acc)
  in
  fun l ->
    let (_, l) = List.fold_left f (Rdf_sparql_ms.MuSet.empty, []) l in
    List.rev l
;;

let slice =
  let rec until len acc i = function
    [] -> List.rev acc
  | _ when i >= len -> List.rev acc
  | h :: q -> until len (h::acc) (i+1) q
  in
  let rec iter start len i = function
    [] -> []
  | h :: q when i < start -> iter start len (i+1) q
  | q ->
      match len with
        None -> q
      | Some len -> until len [] 0 q
  in
  fun l off lim ->
    match off, lim with
      None, None -> l
    | Some off, None -> iter off None 0 l
    | None, Some lim -> until lim [] 0 l
    | Some off, Some lim -> iter off (Some lim) 0 l
;;

let group_omega =
  let make_e expr = { expr_loc = Rdf_loc.dummy_loc ; expr } in
  let map_conds = function
  | GroupBuiltInCall c -> make_e (EBic c)
  | GroupFunctionCall c -> make_e (EFuncall c)
  | GroupVar gv ->
      match gv.grpvar_expr, gv.grpvar with
        None, None -> assert false
      | Some e, None -> e
      | None, Some v -> make_e (EVar v)
      | Some e, Some v -> assert false (* what to evaluate ? *)
  in
  let eval_one ctx mu e =
    try Some(Rdf_dt.to_term (eval_expr ctx mu e))
    with _ -> None
  in

  fun ctx conds o ->
    let conds = List.map map_conds conds in
    let eval ctx mu = List.map (eval_one ctx mu) conds in
    Rdf_sparql_ms.omega_fold
      (fun mu acc ->
         let v = eval ctx mu in
         let o =
           try GExprMap.find v acc
           with Not_found -> Rdf_sparql_ms.Multimu.empty
         in
         let o = Rdf_sparql_ms.omega_add mu o in
         GExprMap.add v o acc
      )
      o
      GExprMap.empty


let agg_count ctx d ms eopt =
  let f mu (muset, vset, n) =
    match eopt with
      None ->
        if d then
          if Rdf_sparql_ms.MuSet.mem mu muset then
            (muset, vset, n)
          else
            (Rdf_sparql_ms.MuSet.add mu muset, vset, n+1)
        else
          (muset, vset, n+1)
    | Some e ->
        match eval_expr ctx mu e with
          Err _ -> (muset, vset, n)
        | v ->
            if d then
              if Rdf_dt.VSet.mem v vset then
                (muset, vset, n)
              else
                (muset, Rdf_dt.VSet.add v vset, n+1)
            else
              (muset, vset, n+1)
  in
  let (_, _, n) = Rdf_sparql_ms.omega_fold f ms (Rdf_sparql_ms.MuSet.empty, Rdf_dt.VSet.empty, 0) in
  dbg ~level: 2 (fun () -> "COUNT(...)="^(string_of_int n));
  Int n
;;

let agg_sum ctx d ms e =
  let f mu (vset, v) =
    match eval_expr ctx mu e with
      Err _ -> (vset, v)
    | v2 ->
        if d then
          if Rdf_dt.VSet.mem v2 vset then
            (vset, v)
          else
            (Rdf_dt.VSet.add v2 vset, eval_plus (v, v2))
        else
          (vset, eval_plus (v, v2))
  in
  let (_, v) = Rdf_sparql_ms.omega_fold f ms (Rdf_dt.VSet.empty, Int 0) in
  v
;;

let agg_fold g base ctx d ms e =
  let f mu (vset, v) =
    let v2 = eval_expr ctx mu e in
    if d then
      if Rdf_dt.VSet.mem v2 vset then
        (vset, v)
      else
        (Rdf_dt.VSet.add v2 vset, g v v2)
    else
      (vset, g v v2)
  in
  let (_, v) = Rdf_sparql_ms.omega_fold f ms (Rdf_dt.VSet.empty, base) in
  v
;;

let agg_min =
  let g v1 v2 =
    match v1, v2 with
      Err _, _ -> v2
    | _, Err _ -> v1
    | _, _ ->
      if sortby_compare v1 v2 > 0 then v2 else v1
  in
  agg_fold g (Err (Rdf_dt.Exception (Error (Empty_set "MIN"))));;

let agg_max =
  let g v1 v2 =
    match v1, v2 with
      Err _, _ -> v2
    | _, Err _ -> v1
    | _, _ ->
      if sortby_compare v1 v2 > 0 then v1 else v2
  in
  agg_fold g (Err (Rdf_dt.Exception (Error (Empty_set "MAX"))));;

let agg_avg ctx d ms e =
  let f mu (vset, v, cpt) =
    match eval_expr ctx mu e with
      Err _ -> (vset, v, cpt)
    | v2 ->
        if d then
          if Rdf_dt.VSet.mem v2 vset then
            (vset, v, cpt)
          else
            (Rdf_dt.VSet.add v2 vset, eval_plus (v, v2), cpt+1)
        else
          (vset, eval_plus (v, v2), cpt+1)
  in
  let (_, v,cpt) = Rdf_sparql_ms.omega_fold f ms (Rdf_dt.VSet.empty, Int 0, 0) in
  match cpt with
    0 -> Int 0
  | _ -> eval_div (v, Int cpt)
;;

let agg_sample ctx d ms e = assert false
let agg_group_concat ctx d ms e sopt =
  let sep = match sopt with None -> " " | Some s -> s in
  let g current v =
    try
      match Rdf_dt.string v with
        Err _ -> current
      | String s ->
          (match current with
             None -> Some s
           | Some cur -> Some (cur ^ sep ^ s)
          )
      | _ -> assert false
    with _ -> current
  in
  match agg_fold g None ctx d ms e with
    None -> String ""
  | Some s -> String s
;;

let eval_agg ctx agg ms =
  match agg with
    Bic_COUNT (d, eopt) -> agg_count ctx d ms eopt
  | Bic_SUM (d, e) -> agg_sum ctx d ms e
  | Bic_MIN (d, e) -> agg_min ctx d ms e
  | Bic_MAX (d, e) -> agg_max ctx d ms e
  | Bic_AVG (d, e) -> agg_avg ctx d ms e
  | Bic_SAMPLE (d, e) ->
      let (_,sample_mu) =
        try Rdf_sparql_ms.Multimu.choose ms
        with Not_found -> assert false
      in
      eval_expr ctx sample_mu e
  | Bic_GROUP_CONCAT (d, e, s_opt) -> agg_group_concat ctx d ms e s_opt
;;
let aggregation ctx agg groups =
  let f ms = eval_agg ctx agg ms in
  GExprMap.map f groups
;;

let aggregate_join =
  let compute_agg ctx ms (i,acc_mu) = function
    Aggregation agg ->
      let term = Rdf_dt.to_term (eval_agg ctx agg ms) in
      let var = "__agg"^(string_of_int i) in
      (i+1, Rdf_sparql_ms.mu_add var term acc_mu)
  | _ -> assert false
  in
  let compute_group ctx aggs key ms acc =
    let (_,mu) = List.fold_left (compute_agg ctx ms) (1,Rdf_sparql_ms.mu_0) aggs in
    Rdf_sparql_ms.omega_add mu acc
  in
  fun eval ctx (conds, a) aggs ->
    let o = eval ctx a in
    let groups = group_omega ctx conds o in
    GExprMap.fold (compute_group ctx aggs) groups Rdf_sparql_ms.Multimu.empty

let cons h q = h :: q ;;

let __print_mu mu =
  Rdf_sparql_ms.SMap.iter
    (fun name term -> print_string (name^"->"^(Rdf_term.string_of_term term)^" ; "))
    mu.Rdf_sparql_ms.mu_bindings;
  print_newline ()
;;

let __print_omega o =
  Rdf_sparql_ms.omega_iter __print_mu o;;


let eval_datablock =
  let mu_add = Rdf_sparql_ms.mu_add in
  let add_var_value mu v = function
  | DataBlockValueIri (Iriref ir) -> mu_add v.var_name (Rdf_term.Iri ir.ir_iri) mu
  | DataBlockValueRdf lit
  | DataBlockValueNumeric lit
  | DataBlockValueBoolean lit ->
      let lit = lit.rdf_lit in
      mu_add v.var_name (Rdf_term.Literal lit) mu
  | DataBlockValueUndef -> mu
  | DataBlockValueIri (PrefixedName _) -> assert false
  | DataBlockValueIri (Reliri _) -> assert false
  in

  let one_var =
    let f var acc dbv =
      let mu = Rdf_sparql_ms.mu_0 in
      let mu = add_var_value mu var dbv in
      Rdf_sparql_ms.omega_add mu acc
    in
    fun v data ->
      List.fold_left (f v) Rdf_sparql_ms.Multimu.empty data
  in
  let full_data =
    let f_row vars acc = function
      Nil -> Rdf_sparql_ms.omega_add Rdf_sparql_ms.mu_0 acc
    | Value dbv_list ->
        let mu = Rdf_sparql_ms.mu_0 in
        let mu = List.fold_left2 add_var_value mu vars dbv_list in
        Rdf_sparql_ms.omega_add mu acc
    in
    fun vars values ->
      List.fold_left (f_row vars) Rdf_sparql_ms.Multimu.empty values
  in
  function
    InLineDataOneVar { idov_var = var ; idov_data = data } ->
      one_var var data
  | InLineDataFull ({ idf_vars = vars ; idf_values = values } as idf) ->
      try full_data vars values
      with
        Invalid_argument "List.fold_left2" ->
          error (Missing_values_in_inline_data idf)

;;


let rec eval ctx = function
| BGP triples ->
    let module M = (val ctx.active.Rdf_graph.bgp : Rdf_bgp.S) in
    let om = M.eval_bgp triples in
    (*prerr_endline "BGP:"; __print_omega om;*)
    om
| Join (BGP [], a) -> eval ctx a
| Join (a1, a2) ->
    let o1 = eval ctx a1 in
    let o2 = eval ctx a2 in
    let o = join_omega ctx o1 o2 in
    (* prerr_endline "JOIN:"; __print_omega o;*)
    o

| LeftJoin (a1, a2, filters) ->
    let o1 = eval ctx a1 in
    let o2 = eval ctx a2 in
    leftjoin_omega ctx o1 o2 filters

| Filter (a, filters) ->
      let omega = eval ctx a in
      filter_omega ctx filters omega

| Union (a1, a2) ->
    let o1 = eval ctx a1 in
    let o2 = eval ctx a2 in
    union_omega o1 o2

| Graph (VIIri (PrefixedName _), _) -> assert false
| Graph (VIIri (Reliri _), _) -> assert false
| Graph (VIIri (Iriref ir), a) ->
    let iri = ir.ir_iri in
    let ctx =
      let g = ctx.dataset.Rdf_ds.get_named iri in
      { ctx with active = g }
    in
    eval ctx a

| Graph (VIVar v, a) ->
      let f_iri iri acc_ms =
        let omega =
          let ctx =
            let g = ctx.dataset.Rdf_ds.get_named iri in
            { ctx with active = g }
          in
          eval ctx a
        in
        let f_mu mu o =
          dbg ~level: 2 (fun () -> ("Add var "^v.var_name^" with value "^(Rdf_iri.string iri)));
          let mu = Rdf_sparql_ms.mu_add v.var_name (Rdf_term.Iri iri) mu in
          Rdf_sparql_ms.omega_add mu o
        in
        let omega = Rdf_sparql_ms.omega_fold f_mu omega Rdf_sparql_ms.Multimu.empty in
        Rdf_sparql_ms.omega_union acc_ms omega
      in
      Iriset.fold f_iri ctx.named Rdf_sparql_ms.Multimu.empty

| Extend (a, var, expr) ->
    let o = eval ctx a in
    extend_omega ctx o var expr

| Minus (a1, a2) ->
    let o1 = eval ctx a1 in
    let o2 = eval ctx a2 in
    minus_omega o1 o2

| ToMultiset a ->
    let l = eval_list ctx a in
    List.fold_left
      (fun o mu -> Rdf_sparql_ms.omega_add mu o)
      Rdf_sparql_ms.Multimu.empty l

| AggregateJoin (Group(conds,a), l) ->
    aggregate_join eval ctx (conds,a) l

| AggregateJoin _ -> assert false (* AggregationJoin always has a Group *)
| Aggregation _ -> assert false (* Aggregation always below AggregateJoin *)
| Group (conds, a) -> assert false (* no group without AggregationJoin above *)

| DataToMultiset datablock -> eval_datablock datablock
| Project _ -> assert false
| Distinct a -> assert false
| Reduced a -> assert false
| Slice (a, offset, limit) -> assert false
| OrderBy (a, order_conds) -> assert false

and eval_list ctx = function
  | OrderBy (a, order_conds) ->
      let l = eval_list ctx a in
      sort_sequence ctx order_conds l
  | Project (a, vars) ->
      let l = eval_list ctx a in
      project_sequence vars l
  | Distinct a ->
      let l = eval_list ctx a in
      distinct l
  | Reduced a ->
      let l = eval_list ctx a in
      distinct l (* FIXME: still have to understand what Reduced means *)
  | Slice (a, off, lim) ->
      let l = eval_list ctx a in
      slice l off lim
  | a ->
      let o = eval ctx a in
      Rdf_sparql_ms.omega_fold cons o []
;;

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

open Rdf_term

type error =
    | Type_error of value * string
    | Invalid_literal of Rdf_term.literal
    | Exception of exn

and value =
  | Err of error
  | Blank of string
  | Iri of Rdf_iri.iri
  | String of string
  | Int of int
  | Float of float
  | Bool of bool
  | Datetime of Netdate.t
  | Ltrl of string * string option (* optional language *)
  | Ltrdt of string * Rdf_iri.iri (* datatyped literal, with unsupported datatype *)

exception Error of error
let error e = raise (Error e)

let date_fmt = "%d %b %Y %T %z"

let string_of_value = function
  Err _ -> "<err>"
| Blank id -> "_:"^id
| Iri iri -> "<"^(Rdf_iri.string iri)^">"
| String s -> Rdf_term.quote_str s
| Int n -> string_of_int n
| Float f -> string_of_float f
| Bool true -> "true"
| Bool false -> "false"
| Datetime t -> Netdate.format ~fmt: date_fmt t
| Ltrl (s,None) -> Rdf_term.quote_str s
| Ltrl (s, Some lang) -> (Rdf_term.quote_str s)^"@"^lang
| Ltrdt (s, iri) -> (Rdf_term.quote_str s)^"^^"^(Rdf_iri.string iri)
;;

module ValueOrdered =
  struct
    type t = value
    let compare v1 v2 =
      match v1, v2 with
        Err e1, Err e2 -> Pervasives.compare e1 e2
      | Err _, _ -> 1
      | _, Err _ -> -1
      | Blank l1, Blank l2 -> String.compare l1 l2
      | Blank _, _ -> 1
      | _, Blank _ -> -1
      | Iri iri1, Iri iri2 -> Rdf_iri.compare iri1 iri2
      | Iri _, _ -> 1
      | _, Iri _ -> -1
      | String s1, String s2
      | Ltrl (s1, None), Ltrl (s2, None)
      | String s1, Ltrl (s2, None)
      | Ltrl (s1, None), String s2 -> String.compare s1 s2
      | String _, _ -> 1
      | _, String _ -> -1
      | Ltrl (s1, Some l1), Ltrl (s2, Some l2) ->
          begin
            match String.compare s1 s2 with
              0 -> String.compare l1 l2
            | n -> n
          end
      | Ltrl _, _ -> 1
      | _, Ltrl _ -> -1
      | Int n1, Int n2 -> n1 - n2
      | Int _, _ -> 1
      | _, Int _ -> -1
      | Float f1, Float f2 -> Pervasives.compare f1 f2
      | Float _, _ -> 1
      | _, Float _ -> -1
      | Bool b1, Bool b2 -> Pervasives.compare b1 b2
      | Bool _, _ -> 1
      | _, Bool _ -> -1
      | Datetime d1, Datetime d2 -> Pervasives.compare (Netdate.since_epoch d1) (Netdate.since_epoch d2)
      | Datetime _, _ -> 1
      | _, Datetime _ -> -1
      | Ltrdt (s1, iri1), Ltrdt (s2, iri2) ->
          (match String.compare s1 s2 with
             0 -> Rdf_iri.compare iri1 iri2
           | n -> n)
       (*| Ltrdt _, _ -> 1
         | _, Ltrdt _ -> -1*)
  end

module VMap = Map.Make (ValueOrdered)
module VSet = Set.Make (ValueOrdered)


let rec string_of_error = function
| Type_error (v, str) ->
   "Value "^(string_of_value v)^" has not the expected type "^str
| Invalid_literal lit ->
   "Invalid literal "^(Rdf_term.string_of_literal lit)
| Exception (Error e) ->
    string_of_error e
| Exception e ->
   let s =
     match e with
       Failure s -> "Failure: "^s
     | e -> Printexc.to_string e
    in
    "Exception "^s
;;

let iri base_iri = function
| Err e -> Err e
| Iri iri -> Iri iri
| (String s)
| (Ltrl (s, None)) as v ->
    begin
      try
        (* FIXME: resolve relative IRI *)
        let iri = Rdf_iri.iri s in
        (*let netu = Rdf_iri.neturl iri in
        let base = Rdf_iri.neturl base_iri in
        let iri = Rdf_iri.of_neturl (Neturl.ensure_absolute_url ~base netu) in*)
        Iri iri
      with _ -> Err (Type_error (v, "iri"))
    end
| v -> Err (Type_error (v, "iri"))
;;

let datatype = function
  Err e -> Err e
| (Blank _)
| (Iri _) as v -> Err (Type_error (v, "literal"))
| v ->
    let iri =
      match v with
        String _ -> Rdf_rdf.xsd_string
      | Int _ -> Rdf_rdf.xsd_integer
      | Float _ -> Rdf_rdf.xsd_double
      | Bool _ -> Rdf_rdf.xsd_boolean
      | Datetime _ -> Rdf_rdf.xsd_datetime
      | Ltrl (_, None) -> Rdf_rdf.xsd_string
      | Ltrl (s, Some _) -> Rdf_rdf.rdf_langstring
      | Ltrdt (_, iri) -> iri
      | Err _ | Blank _ | Iri _ -> assert false
    in
    Iri iri
;;

let string_literal v =
  match v with
  String s -> (s, None)
| Ltrl (s, lang) -> (s, lang)
| Ltrdt _ | Int _ | Float _ | Bool _ | Blank _
| Datetime _ | Iri _ | Err _ ->
      error (Type_error (v, "literal_string"))
;;

let string = function
  Err e -> Err e
| v ->
    let s =
      match v with
      | Err e -> assert false
      | Iri t -> Rdf_iri.string t
      | String s -> s
      | Int n -> string_of_int n
      | Float f -> string_of_float f
      | Bool true -> "true"
      | Bool false -> "false"
      | Datetime t -> Netdate.format ~fmt: date_fmt t
      | Ltrl (s, _) -> s
      | Blank _ -> error (Type_error (v, "string"))
      | Ltrdt (s, _) -> s
    in
    String s

let int = function
| Err e -> Err e
| v ->
    try
      let n =
        match v with
        | Err e -> assert false
        | String s
        | Ltrdt (s, _)
        | Ltrl (s, _) -> int_of_string s
        | Int n -> n
        | Float f -> truncate f
        | Bool true -> 1
        | Bool false -> 0
        | Datetime _
        | Iri _ | Blank _ -> failwith ""
      in
      Int n
    with
      _ -> error (Type_error (v, "int"))
;;

let float = function
| Err e -> Err e
| v ->
    try
      let f =
        match v with
        | Err e ->  assert false
        | String s
        | Ltrdt (s, _)
        | Ltrl (s, _) -> float_of_string s
        | Int n -> float n
        | Float f -> f
        | Bool true -> 1.0
        | Bool false -> 0.0
        | Datetime _
        | Iri _ | Blank _ -> failwith ""
      in
      Float f
    with
      _ -> error (Type_error (v, "float"))
;;


let bool = function
| Err e -> Err e
| v ->
    try
      let b =
        match v with
        | Err e -> assert false
        | String s
        | Ltrdt (s, _)
        | Ltrl (s, _) ->
            (match s with
               "1" | "true" -> true
             | "0" | "false" -> false
             | _ -> failwith ""
            )
        | Int n -> n <> 0
        | Float f ->
            (match classify_float f with
               FP_zero | FP_nan -> false
             |_ -> true
          )
        | Bool b -> b
        | Datetime _
        | Iri _ | Blank _ -> failwith ""
      in
      Bool b
    with
      _ -> error (Type_error (v, "bool"))
;;

let datetime = function
| Err e -> Err e
| v ->
    try
      let t =
        match v with
        | Err e -> assert false
        | String s
        | Ltrdt (s, _)
        | Ltrl (s, _) -> Netdate.parse s
        | Int _ | Float _ | Bool _  | Iri _  | Blank _ -> failwith ""
        | Datetime t -> t
      in
      Datetime t
  with
      _ -> error (Type_error (v, "datetime"))
;;

let ltrl = function
| Err e -> Err e
| v ->
    try
      let (s, lang) =
        match v with
        | Err e -> assert false
        | Iri t -> (Rdf_iri.string t, None)
        | String s
        | Ltrdt (s, _) -> (s, None)
        | Ltrl (s, l) -> (s, l)
        | Int n -> (string_of_int n, None)
        | Float f -> (string_of_float f, None)
        | Bool true -> ("true", None)
        | Bool false -> ("false", None)
        | Datetime t -> (Netdate.format ~fmt: date_fmt t, None)
        | Blank _ -> error (Type_error (v, "ltrl"))
      in
      Ltrl (s, lang)
    with
      _ -> error (Type_error (v, "ltrl"))
;;

let numeric = function
| Err e -> Err e
| v ->
    try
      match v with
      | Err e -> assert false
      | String s
      | Ltrdt (s, _)
      | Ltrl (s, _) ->
          begin
            try Int (int_of_string s)
            with _ ->
                try Float (float_of_string s)
                with _ -> failwith ""
          end
      | Int n -> Int n
      | Float f -> Float f
      | Bool true -> Int 1
      | Bool false -> Int 0
      | Datetime _ | Iri _ | Blank _ -> failwith ""
    with
      _ -> error (Type_error (v, "numeric"))
;;

let of_literal lit =
  try
    match lit.lit_type with
    | Some t when Rdf_iri.equal t Rdf_rdf.xsd_boolean ->
        bool (String lit.lit_value)
    | Some t when Rdf_iri.equal t Rdf_rdf.xsd_integer ->
        begin
          try Int (int_of_string lit.lit_value)
          with _ -> failwith ""
        end
    | Some t when Rdf_iri.equal t Rdf_rdf.xsd_double
          or Rdf_iri.equal t Rdf_rdf.xsd_decimal ->
        begin
          try Float (float_of_string lit.lit_value)
          with _ -> failwith ""
        end
    | Some t when Rdf_iri.equal t Rdf_rdf.xsd_string ->
        String lit.lit_value
    | Some t when Rdf_iri.equal t Rdf_rdf.xsd_datetime ->
        Datetime (Netdate.parse lit.lit_value)
    | None ->
        begin
          match lit.lit_language with
            None -> String lit.lit_value
          | Some s -> Ltrl (lit.lit_value, Some s)
        end
    | Some dt ->
        Ltrdt (lit.lit_value, dt)
  with
  _ -> error (Invalid_literal lit)

let of_term = function
  Rdf_term.Iri t -> Iri t
| Rdf_term.Literal lit -> of_literal lit
| Rdf_term.Blank_ label -> Blank (Rdf_term.string_of_blank_id label)
| Rdf_term.Blank -> assert false

let to_term = function
| Err e -> error e
| Iri t -> Rdf_term.Iri t
| Blank label -> Rdf_term.Blank_ (Rdf_term.blank_id_of_string label)
| String s -> Rdf_term.term_of_literal_string ~typ: Rdf_rdf.xsd_string s
| Int n -> Rdf_term.term_of_int n
| Float f -> Rdf_term.term_of_double f
| Bool b -> Rdf_term.term_of_bool b
| Datetime d -> Rdf_term.term_of_datetime ~d: (Netdate.since_epoch d) ()
| Ltrl (s,lang) -> Rdf_term.term_of_literal_string ?lang s
| Ltrdt (s, typ) -> Rdf_term.term_of_literal_string ~typ s


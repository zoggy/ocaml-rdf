(*********************************************************************************)
(*                OCaml-RDF                                                      *)
(*                                                                               *)
(*    Copyright (C) 2012-2016 Institut National de Recherche en Informatique     *)
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
  | Iri of Iri.t
  | String of string
  | Int of int * Iri.t
  | Float of float
  | Bool of bool
  | HexBinary of string
  | Datetime of Rdf_term.datetime
  | Ltrl of string * string option (* optional language *)
  | Ltrdt of string * Iri.t (* datatyped literal, with unsupported datatype *)

exception Error of error
let error e = raise (Error e)

let string_of_value = function
  Err _ -> "<err>"
| Blank id -> "_:"^id
| Iri iri -> "<"^(Iri.to_string iri)^">"
| String s -> Rdf_term.quote_str s
| Int (n,_) -> string_of_int n
| Float f -> string_of_float f
| Bool true -> "true"
| Bool false -> "false"
| HexBinary s -> Rdf_term.quote_str s
| Datetime t -> Rdf_term.string_of_datetime t
| Ltrl (s,None) -> Rdf_term.quote_str s
| Ltrl (s, Some lang) -> (Rdf_term.quote_str s)^"@"^lang
| Ltrdt (s, iri) -> (Rdf_term.quote_str s)^"^^"^(Iri.to_string iri)
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
      | Iri iri1, Iri iri2 -> Iri.compare iri1 iri2
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
      | Int (n1,_), Int (n2,_) -> n1 - n2
      | Int _, _ -> 1
      | _, Int _ -> -1
      | Float f1, Float f2 -> Pervasives.compare f1 f2
      | Float _, _ -> 1
      | _, Float _ -> -1
      | Bool b1, Bool b2 -> Pervasives.compare b1 b2
      | Bool _, _ -> 1
      | _, Bool _ -> -1
      | HexBinary h1, HexBinary h2 -> Pervasives.compare h1 h2
      | HexBinary _, _ -> 1
      | _, HexBinary _ -> -1
      | Datetime d1, Datetime d2 ->
          Ptime.compare d1.stamp d2.stamp
      | Datetime _, _ -> 1
      | _, Datetime _ -> -1
      | Ltrdt (s1, iri1), Ltrdt (s2, iri2) ->
          (match String.compare s1 s2 with
             0 -> Iri.compare iri1 iri2
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
      | Iri.Error e -> Iri.string_of_error e
      | e -> Printexc.to_string e
    in
    "Exception "^s
;;

let () = Printexc.register_printer
  (function
   | Error e -> Some (string_of_error e)
   | _ -> None)


let iri base_iri = function
| Err e -> Err e
| Iri iri -> Iri iri
| (String s)
| (Ltrl (s, None)) as v ->
    begin
      try Iri (Iri.resolve ~base: base_iri (Iri.of_string s))
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
      | HexBinary _ -> Rdf_rdf.xsd_hexBinary
      | Datetime _ -> Rdf_rdf.xsd_datetime
      | Ltrl (_, None) -> Rdf_rdf.xsd_string
      | Ltrl (s, Some _) -> Rdf_rdf.dt_langString
      | Ltrdt (_, iri) -> iri
      | Err _ | Blank _ | Iri _ -> assert false
    in
    Iri iri
;;

let string_literal v =
  match v with
  String s -> (s, None)
| Ltrl (s, lang) -> (s, lang)
| Ltrdt _ | Int _ | Float _ | Bool _ | HexBinary _ | Blank _
| Datetime _ | Iri _ | Err _ ->
      error (Type_error (v, "literal_string"))
;;

let string = function
  Err e -> Err e
| v ->
    let s =
      match v with
      | Err e -> assert false
      | Iri t -> Iri.to_string t
      | String s -> s
      | Int (n,_) -> string_of_int n
      | Float f -> string_of_float f
      | Bool true -> "true"
      | Bool false -> "false"
      | HexBinary s -> s
      | Datetime t -> Rdf_term.string_of_datetime t
      | Ltrl (s, _) -> s
      | Blank _ -> error (Type_error (v, "string"))
      | Ltrdt (s, _) -> s
    in
    String s

let int = function
| Err e -> Err e
| v ->
    try
      let (n,dt) =
        match v with
        | Err e -> assert false
        | String s
        | Ltrdt (s, _)
        | Ltrl (s, _) -> (int_of_string s, None)
        | Int (x,dt) -> (x,Some dt)
        | Float f -> (truncate f, None)
        | Bool true -> (1,None)
        | Bool false -> (0,None)
        | HexBinary s -> (int_of_string ("0x"^s), None)
        | Datetime _
        | Iri _ | Blank _ -> failwith ""
      in
      let dt = match dt with None -> Rdf_rdf.xsd_int | Some dt -> dt in
      Int (n,dt)
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
        | Int (n, _) -> float n
        | Float f -> f
        | Bool true -> 1.0
        | Bool false -> 0.0
        | HexBinary _
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
        | Int (n, _) -> n <> 0
        | Float f ->
            (match classify_float f with
               FP_zero | FP_nan -> false
             |_ -> true
          )
        | Bool b -> b
        | HexBinary _
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
        | Ltrl (s, _) -> Rdf_term.datetime_of_string s
        | Int _ | Float _ | Bool _  | HexBinary _ | Iri _  | Blank _ -> failwith ""
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
        | Iri t -> (Iri.to_string t, None)
        | String s
        | Ltrdt (s, _) -> (s, None)
        | Ltrl (s, l) -> (s, l)
        | Int (n, _) -> (string_of_int n, None)
        | Float f -> (string_of_float f, None)
        | Bool true -> ("true", None)
        | Bool false -> ("false", None)
        | HexBinary s -> (s, None)
        | Datetime t -> (Rdf_term.string_of_datetime t, None)
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
            try Int (int_of_string s, Rdf_rdf.xsd_int)
            with _ ->
                try Float (float_of_string s)
                with _ -> failwith ""
          end
      | Int _ as x -> x
      | Float f -> Float f
      | Bool true -> Int (1, Rdf_rdf.xsd_int)
      | Bool false -> Int (0, Rdf_rdf.xsd_int)
      | HexBinary s -> (try Int (int_of_string ("0x"^s), Rdf_rdf.xsd_int) with _ -> failwith "")
      | Datetime _ | Iri _ | Blank _ -> failwith ""
    with
      _ -> error (Type_error (v, "numeric"))
;;

let of_literal lit =
  try
    match lit.lit_type with
    | Some t when Iri.equal t Rdf_rdf.xsd_boolean ->
        bool (String lit.lit_value)
    | Some t when Iri.Set.mem  t Rdf_rdf.integer_types ->
        begin
          try Int (int_of_string lit.lit_value, t)
          with _ -> failwith ""
        end
    | Some t when Iri.equal t Rdf_rdf.xsd_double
          || Iri.equal t Rdf_rdf.xsd_decimal ->
        begin
          try Float (float_of_string lit.lit_value)
          with _ -> failwith ""
        end
    | Some t when Iri.equal t Rdf_rdf.xsd_hexBinary ->
        HexBinary (String.lowercase_ascii lit.lit_value)
    | Some t when Iri.equal t Rdf_rdf.xsd_string ->
        String lit.lit_value
    | Some t when Iri.equal t Rdf_rdf.xsd_datetime ->
        Datetime (Rdf_term.datetime_of_string lit.lit_value)
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
| Int (n, typ) -> Rdf_term.term_of_int ~typ n
| Float f -> Rdf_term.term_of_double f
| Bool b -> Rdf_term.term_of_bool b
| HexBinary s -> Rdf_term.term_of_literal_string ~typ: Rdf_rdf.xsd_hexBinary s
| Datetime d -> Rdf_term.term_of_datetime ~d ()
| Ltrl (s,lang) -> Rdf_term.term_of_literal_string ?lang s
| Ltrdt (s, typ) -> Rdf_term.term_of_literal_string ~typ s


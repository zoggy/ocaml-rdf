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

module SMap = Map.Make(String)
module J = Yojson.Safe

type type_coercion = Id | Vocab

type context_entry =
  { id : Iri.t ;
    typ : type_coercion option ;
  }

type context = {
    map : context_entry SMap.t ;
    base : Iri.t ;
    language : string option ;
    vocab : Iri.t option ;
  }

type context_cache = {
      mutable map : context Iri.Map.t ;
    }

module Ast =
  struct
    type loc = int * int (* 1-based line * 0-based column *)
    type range = loc * loc

    type 'a locd = { loc : range option ; data : 'a }

    type term_type = [
      | `String of string
      | `Iri of Iri.t
      | `Blank of string
      ]

    type term =
      { term_loc : range option;
        term_type : term_type ;
      }

    type ctx_def =
      { ctx_loc : range option;

      }

    type ctx_ref_type = [
      | `Iri of Iri.t
      | `Ctx_def of ctx_def
      | `Array of ctx_ref list
      ]

    and ctx_ref =
      { ctxr_loc : range ;
        ctxr_type : ctx_ref_type ;
      }

    type id_type = [
      | `Compact of string locd * string locd
      | `Iri of Iri.t locd
      | `Blank of string locd
      ]

    type id =
      { id_loc : range ;
        id_type : id_type ;
      }

    type node_type = [
      | `Term of term
      | `Compact of string locd * string locd
      | `Iri of Iri.t locd
      | `Blank of string locd
      ]

    type kv_type = [
      | `String of string
      | `Number of int (* TODO *)
      | `Bool of bool
      | `Null
      ]

    type reverse_def = unit (* TODO *)

    and node =
      {
        node_loc : range option;
        node_ctx : ctx_ref option ;
        node_id : id option ;
        node_graph : node list locd option;
        node_type : node_type option ;
        node_reverse : reverse_def list option ;
        node_index : string locd option ;
        node_map : (term * key_value) list ;
      }
    and ext_kv_type = [
      | kv_type
      | `Node of node
      | `Value of value
      | `List of unit (* TODO *)
      | `Set of unit (* TODO *)
      | `Array of ext_kv_type list
      | `Language_map of unit (* TODO *)
      | `Index_map of unit (* TODO *)
      ]

    and key_value =
      { kv_loc : range option ;
        kv_type : ext_kv_type locd ;
      }

    and value =
      {
        val_loc : range option ;
        val_value : kv_type locd ;
        val_type : node_type option ;
        val_lang : string locd option ;
        val_index : string locd option ;
      }


    exception Escape of ((int * int) * (int * int)) * Jsonm.error

    let json_of_src ?encoding
      (src : [`Channel of in_channel | `String of string])
        =
      let dec d = match Jsonm.decode d with
        | `Lexeme l -> l
        | `Error e -> raise (Escape (Jsonm.decoded_range d, e))
        | `End | `Await -> assert false
      in
      let rec value v k d = match v with
        | `Os -> obj [] k d  | `As -> arr [] k d
        | `Null | `Bool _ | `String _ | `Float _ as v -> k v d
        | _ -> assert false
      and arr vs k d = match dec d with
        | `Ae -> k (`List (List.rev vs)) d
        | v -> value v (fun v -> arr (v :: vs) k) d
      and obj ms k d = match dec d with
        | `Oe -> k (`Assoc (List.rev ms)) d
        | `Name n -> value (dec d) (fun v -> obj ((n, v) :: ms) k) d
        | _ -> assert false
      in
      let d = Jsonm.decoder ?encoding src in
      try `JSON (value (dec d) (fun v _ -> v) d) with
      | Escape (r, e) -> `Error (r, e)

    let from_string ?encoding s = json_of_src ?encoding (`String s)
  end

module type IO =
  sig
    type 'a t
    val return : 'a -> 'a t
    val bind : 'a t -> ('a -> 'b t) -> 'b t
    val get : Iri.t -> (J.json, string) result t
  end

module Make(IO:IO) =
  struct

  end

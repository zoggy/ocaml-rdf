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
type container_type = Language | List | Set | Index

type context_entry =
  { id : Iri.t ;
    typ : type_coercion option ;
    container : container_type option ;
    language: string option;
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

type loc = int * int (* 1-based line * 0-based column *)
type range = loc * loc

type 'a ranged = { loc : range option ; data : 'a }
let ranged ?loc data = { loc ; data }

module Json =
  struct
    type key = string ranged
    type json_t = [
      | `Obj of (key * json) list
      | `List of json list
      | `String of string
      | `Bool of bool
      | `Float of float
      | `Null
      ]
    and json = {
      loc : range option;
      t : json_t ;
      }
    let json ?loc t = { loc ; t }

    exception Escape of ((int * int) * (int * int)) * Jsonm.error

    let dec d = match Jsonm.decode d with
      | `Lexeme l -> l
      | `Error e -> raise (Escape (Jsonm.decoded_range d, e))
      | `End | `Await -> assert false

    let loc_start d = fst (Jsonm.decoded_range d)
    let loc_end d = snd (Jsonm.decoded_range d)
    let range d = Jsonm.decoded_range d

    let rec value v k d =
      match v with
      | `Os -> obj (loc_start d) [] k d
      | `As -> arr (loc_start d) [] k d
      | `Null | `Bool _ | `String _ | `Float _ as v ->
          let v = json ~loc: (range d) v in
          k v d
      | _ -> assert false
    and arr loc_start vs k d =
      match dec d with
      | `Ae ->
         let loc = (loc_start, loc_end d) in
         let v = json ~loc (`List (List.rev vs)) in
         k v d
      | v -> value v (fun v -> arr loc_start (v :: vs) k) d
    and obj loc_start ms k d =
      match dec d with
      | `Oe ->
         let loc = (loc_start, loc_end d) in
         let v = json ~loc (`Obj (List.rev ms)) in
         k v d
      | `Name n ->
         let key = ranged n in
         value (dec d) (fun v -> obj loc_start ((key, v) :: ms) k) d
      | _ -> assert false

    let json_of_src ?encoding
      (src : [`Channel of in_channel | `String of string])
      =
      let d = Jsonm.decoder ?encoding src in
      try Ok (value (dec d) (fun v _ -> v) d) with
      | Escape (r, e) -> Error (r, e)

    let enc e l = ignore (Jsonm.encode e (`Lexeme l))
    let rec value v k e = match v with
      | `List vs -> arr vs k e
      | `Obj ms -> obj ms k e
      | `Null | `Bool _ | `Float _ | `String _ as v -> enc e v; k e
    and arr vs k e = enc e `As; arr_vs vs k e
    and arr_vs vs k e = match vs with
      | v :: vs' -> value v.t (arr_vs vs' k) e
      | [] -> enc e `Ae; k e
    and obj ms k e = enc e `Os; obj_ms ms k e
    and obj_ms ms k e = match ms with
      | (key, v) :: ms -> enc e (`Name key.data); value v.t (obj_ms ms k) e
      | [] -> enc e `Oe; k e

    let json_to_dst ~minify
      (dst : [`Channel of out_channel | `Buffer of Buffer.t ])
        (json : json)
        =
      let e = Jsonm.encoder ~minify dst in
      let finish e = ignore (Jsonm.encode e `End) in
      match json.t with `List _ | `Obj _ as json -> value json finish e
      | _ -> invalid_arg "invalid json text"

    let from_string ?encoding s = json_of_src ?encoding (`String s)
    let to_string ?(minify=false) json =
      let b = Buffer.create 256 in
      json_to_dst ~minify (`Buffer b);
      Buffer.contents b
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


    type term_type = [
      | `String of string
      | `Iri of Iri.t
      | `Blank of string
      ]

    type term =
      { term_loc : range option;
        term_type : term_type ;
      }

    type id_type = [
      | `Compact of string ranged * string ranged
      | `Iri of Iri.t ranged
      | `Blank of string ranged
      ]

    type ctx_term = [
      | `String
      | `Compact of string ranged * string ranged
      | `Iri of Iri.t ranged
     ]

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

    type id =
      { id_loc : range ;
        id_type : id_type ;
      }

    type node_type = [
      | `Term of term
      | `Compact of string ranged * string ranged
      | `Iri of Iri.t ranged
      | `Blank of string ranged
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
        node_graph : node list ranged option;
        node_type : node_type option ;
        node_reverse : reverse_def list option ;
        node_index : string ranged option ;
        node_map : (term * key_value) list ;
      }
    and kv_type2 = [
      | kv_type
      | `Node of node
      | `Value of value
      | `Array of kv_type2 list
      ]
    and kv_type3 = [
      | kv_type
      | `Node of node
      | `Value of value
      | `Array of kv_type3 list
      | `List of list_object
      | `Set of set_object
      | `Language_map of language_map
      | `Index_map of index_map
      ]
    and language_map = node (* as we can't know at parsing time whether
      node is a regular one or a language map *)
    and index_map = node (* as we can't know at parsing time whether
      node is a regular one or an index map *)

    and key_value =
      { kv_loc : range option ;
        kv_type : kv_type3 ranged ;
      }

    and value =
      {
        val_loc : range option ;
        val_value : kv_type ranged ;
        val_type : node_type option ;
        val_lang : string ranged option ;
        val_index : string ranged option ;
      }
    and list_object =
      { list_loc : range option ;
        list_ctx : ctx_ref option ;
        list_index : string ranged option ;
        list_value : kv_type2 ranged list ;
      }
    and set_object = list_object
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

module SMap = Rdf_xml.SMap;;

type context =
  { base : Rdf_uri.uri ;
    prefixes : Rdf_uri.uri SMap.t ;
    gstate : Rdf_xml.global_state ;
  }
type uriref = string

type directive =
  | Prefix of string option * uriref
  | Base of uriref

type qname = string option * string option

type resource =
  | Uriref of uriref
  | Qname of qname

type language = string

type literal =
  | String of string * language option * resource option

type object_ =
  | Obj_res of resource
  | Obj_blank of blank
  | Obj_literal of literal

and blank =
 | NodeId of string
 | Empty
 | PredObjs of predobj list
 | Collection of object_ list

and pred = Pred_res of resource | Pred_a

and predobj = pred * object_ list

type subject =
  | Sub_res of resource
  | Sub_blank of blank

type statement =
  Directive of directive
| Triples of subject * predobj list

type turtle = statement list


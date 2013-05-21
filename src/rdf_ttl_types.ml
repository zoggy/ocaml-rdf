(** *)

module SMap = Rdf_xml.SMap;;

type context =
  { base : Rdf_uri.uri ;
    prefixes : Rdf_uri.uri SMap.t ;
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
  | Integer of int
  | Decimal of string
  | Double of string
  | Boolean of bool

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

let uri_of_uriref ctx s =
  let url = Rdf_uri.neturl (Rdf_uri.uri s) in
  let url = Neturl.ensure_absolute_url ~base: (Rdf_uri.neturl ctx.base) url in
  Rdf_uri.of_neturl url
;;

let insert_sub_predobjs ctx g sub l = (ctx, g)

let apply_statement ctx g = function
  Directive (Prefix (s_opt, uri)) ->
    let s = match s_opt with None -> "" | Some s -> s in
    let uri = uri_of_uriref ctx s in
    let ctx = { ctx with prefixes = SMap.add s uri ctx.prefixes } in
    (ctx, g)
| Directive (Base uri) ->
    let uri = uri_of_uriref ctx uri in
    let ctx = { ctx with base = uri } in
    (ctx, g)
| Triples (subject, predobjs) ->
    insert_sub_predobjs ctx g subject predobjs
;;

(** *)

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
  | String of string * language option
  | Datatype_string of string * resource
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
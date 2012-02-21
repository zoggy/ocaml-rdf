(*********************************************************************************)
(*                OCaml-RDF                                                      *)
(*                                                                               *)
(*    Copyright (C) 2011 Institut National de Recherche en Informatique          *)
(*    et en Automatique. All rights reserved.                                    *)
(*                                                                               *)
(*    This program is free software; you can redistribute it and/or modify       *)
(*    it under the terms of the GNU Lesser General Public License version        *)
(*    2.1 or later as published by the Free Software Foundation.                 *)
(*                                                                               *)
(*    This program is distributed in the hope that it will be useful,            *)
(*    but WITHOUT ANY WARRANTY; without even the implied warranty of             *)
(*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the              *)
(*    GNU Library General Public License for more details.                       *)
(*                                                                               *)
(*    You should have received a copy of the GNU Lesser General Public           *)
(*    License along with this program; if not, write to the Free Software        *)
(*    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA                   *)
(*    02111-1307  USA                                                            *)
(*                                                                               *)
(*    Contact: Maxence.Guesdon@inria.fr                                          *)
(*                                                                               *)
(*                                                                               *)
(*********************************************************************************)

(** Nodes (RDF terms).
  @rdfmod redland-node.html
  @rdfprefix librdf_
*)

open Rdf_types;;

(**/**)
let dbg = Rdf_misc.create_log_fun ~prefix: "Rdf_node" "ORDF_NODE";;

module Raw =
  struct
    external free : node -> unit = "ml_librdf_free_node"

    external new_node : world -> node option = "ml_librdf_new_node"

    external new_from_node : node -> node option = "ml_librdf_new_node_from_node"

    external new_from_blank_identifier : world -> string option -> node option =
      "ml_librdf_new_node_from_blank_identifier"

    external new_from_literal : world -> string -> string option -> bool -> node option =
      "ml_librdf_new_node_from_literal"

    external new_from_normalised_uri_string : world -> string -> uri -> uri -> node option =
      "ml_librdf_new_node_from_normalised_uri_string"

    external new_from_typed_literal : world -> string -> string option -> uri option -> node option =
      "ml_librdf_new_node_from_typed_literal"

    external new_from_uri : world -> uri -> node option =
      "ml_librdf_new_node_from_uri"

    external new_from_uri_local_name : world -> uri -> string -> node option =
      "ml_librdf_new_node_from_uri_local_name"

    external new_from_uri_string : world -> string -> node option =
      "ml_librdf_new_node_from_uri_string"

    external equals : node -> node -> bool = "ml_librdf_node_equals"
    external get_blank_identifier : node -> string option = "ml_librdf_node_get_blank_identifier"
    external get_li_ordinal : node -> int = "ml_librdf_node_get_li_ordinal"

    external get_literal_value : node -> string option = "ml_librdf_node_get_literal_value"
    external get_literal_value_as_latin1 : node -> string option =
      "ml_librdf_node_get_literal_value_as_latin1"

    external get_literal_value_datatype_uri : node -> uri option =
      "ml_librdf_node_get_literal_value_datatype_uri"
    external get_literal_value_is_wf_xml : node -> bool =
      "ml_librdf_node_get_literal_value_is_wf_xml"
    external get_literal_value_language : node -> string option =
      "ml_librdf_node_get_literal_value_language"

    external get_type : node -> Rdf_enums.node_type =
      "ml_librdf_node_get_type"

    external get_uri : node -> uri option = "ml_librdf_node_get_uri"

    external is_blank : node -> bool = "ml_librdf_node_is_blank"
    external is_literal : node -> bool = "ml_librdf_node_is_literal"
    external is_resource : node -> bool = "ml_librdf_node_is_resource"

    external print : node -> Unix.file_descr -> unit = "ml_librdf_node_print"

    external pointer_of_node : node -> Nativeint.t = "ml_pointer_of_custom"
  end

let free v =
  dbg (fun () -> Printf.sprintf "Freeing node %s"
   (Nativeint.to_string (Raw.pointer_of_node v)));
  Raw.free v
;;
let to_finalise v = Gc.finalise free v;;
(**/**)

exception Node_creation_failed of string;;

type contents =
  | Uri of uri
  | Literal of string
  | Blank of string
;;

let on_new_node fun_name = function
  None -> raise (Node_creation_failed fun_name)
| Some n -> to_finalise n; n
;;

let on_new_node_opt fun_name = function
  None -> None
| Some n -> to_finalise n; Some n
;;

(** @rdf new_node *)
let new_node world = on_new_node "" (Raw.new_node world);;

(** @rdf new_node_from_node *)
let copy_node node =
  on_new_node "from_node" (Raw.new_from_node node)
;;

(** @rdf new_node_from_blank_identifier *)
let new_from_blank_identifier ?string world =
  on_new_node "from_blank_identifier"
  (Raw.new_from_blank_identifier world string)
;;

(** @rdf new_node_from_literal *)
let new_from_literal world ?xml_language ?(is_wf_xml=false) string =
  on_new_node "from_literal"
  (Raw.new_from_literal world string xml_language is_wf_xml)
;;

(** @rdf new_node_from_normalised_uri_string *)
let new_from_normalised_uri_string world ~uri ~source ~base =
  on_new_node "from_normalised_uri_string"
  (Raw.new_from_normalised_uri_string world uri source base)
;;

(** @rdf new_node_from_typed_literal *)
let new_from_typed_literal world ?xml_language ?datatype value =
  on_new_node "from_typed_literal"
  (Raw.new_from_typed_literal world value xml_language datatype)
;;

(** @rdf new_node_from_uri *)
let new_from_uri world uri =
  on_new_node "from_uri"
  (Raw.new_from_uri world uri)
;;

(** @rdf new_node_from_uri_local_name *)
let new_from_uri_local_name world uri name =
  on_new_node "from_uri_local_name"
  (Raw.new_from_uri_local_name world uri name)
;;

(** @rdf new_node_from_uri_string *)
let new_from_uri_string world string =
  on_new_node "from_uri_string"
  (Raw.new_from_uri_string world string)
;;

(** @rdf node_get_uri *)
let get_uri node =
  Rdf_misc.map_opt Rdf_uri.copy_uri (Raw.get_uri node)
;;

(** @rdf node_get_literal_value_datatype *)
let get_literal_value_datatype_uri node =
  Rdf_misc.map_opt Rdf_uri.copy_uri
  (Raw.get_literal_value_datatype_uri node)
;;

(** @rdf node_print *)
let print = Raw.print;;

(** @rdf node_equals *)
let equals = Raw.equals

(** @rdf node_get_blank_identifier *)
let get_blank_identifier = Raw.get_blank_identifier

(** @rdf node_get_li_ordinal *)
let get_li_ordinal node =
  let n = Raw.get_li_ordinal node in
  if n < 0 then failwith "node_get_li_ordinal";
  n
;;

(** @rdf node_get_literal_value *)
let get_literal_value = Raw.get_literal_value;;

(** @rdf node_get_literal_value_as_latin1 *)
let get_literal_value_as_latin1 = Raw.get_literal_value_as_latin1;;

(** @rdf node_get_literal_value_datatype_uri *)
let get_literal_value_datatype_uri node =
  Rdf_misc.map_opt Rdf_uri.copy_uri
   (Raw.get_literal_value_datatype_uri node);;

(** @rdf node_get_literal_value_is_wf_xml *)
let get_literal_value_is_wf_xml = Raw.get_literal_value_is_wf_xml;;

(** @rdf node_get_literal_value_language *)
let get_literal_value_language = Raw.get_literal_value_language;;

(** @rdf node_get_type *)
let get_type = Raw.get_type;;

(** @rdf node_is_blank *)
let is_blank = Raw.is_blank;;

(** @rdf node_is_literal *)
let is_literal = Raw.is_literal;;

(** @rdf node_is_resource *)
let is_resource = Raw.is_resource;;

(** [kind node] returns a [contents] or raise [Failure] in
  case of error. *)
let kind node =
  if is_resource node then
    match get_uri node with
      None -> failwith "Node is resource but could not get uri"
    | Some uri -> Uri uri
  else
    if is_blank node then
      match get_blank_identifier node with
        None -> failwith "Node is blank but could not get identifier"
      | Some s -> Blank s
    else
      if is_literal node then
        match get_literal_value node with
          None -> failwith "Node is literal but could not get value"
        | Some s -> Literal s
      else
        failwith "Node is not a ressource, not a blank, nor a literal"
;;

let to_string node =
  match kind node with
    Uri uri -> Printf.sprintf "<%s>" (Rdf_uri.as_string uri)
  | Literal s -> s
  | Blank s -> Printf.sprintf "_:%s" s
;;


(** *)

open Rdf_types;;

external new_node : world -> node option = "ml_librdf_new_node"
external free_node : node -> unit = "ml_librdf_free_node"

external new_node_from_blank_identifier : world -> string option -> node option =
  "ml_librdf_new_node_from_blank_identifier"

external new_node_from_literal : world -> string -> string option -> bool -> node option =
  "ml_librdf_new_node_from_literal"

external new_node_from_normalised_uri_string : world -> string -> uri -> uri -> node option =
  "ml_librdf_new_node_from_normalised_uri_string"

external new_node_from_typed_literal : world -> string -> string option -> uri option -> node option =
  "ml_librdf_new_node_from_typed_literal"

external new_node_from_uri : world -> uri -> node option =
  "ml_librdf_new_node_from_uri"

external new_node_from_uri : world -> uri -> string -> node option =
  "ml_librdf_new_node_from_uri_local_name"

external new_node_from_uri_string : world -> string -> node option =
  "ml_librdf_new_node_from_uri_string"

external node_equals : node -> node -> bool = "ml_librdf_node_equals"
external node_get_blank_identifier : node -> string = "ml_librdf_node_get_blank_identifier"
external node_get_li_ordinal : node -> int = "ml_librdf_node_get_li_ordinal"

external node_get_literal_value : node -> string option = "ml_librdf_node_get_literal_value"
external node_get_literal_value_as_latin1 : node -> string option =
  "ml_librdf_node_get_literal_value_as_latin1"
external node_get_literal_value_datatype_uri : node -> uri option =
  "ml_librdf_node_get_literal_value_datatype_uri"
external node_get_literal_value_is_wf_xml : node -> bool =
  "ml_librdf_node_get_literal_value_is_wf_xml"
external node_get_literal_value_language : node -> string option =
  "ml_librdf_node_get_literal_value_language"

external node_get_type : node -> Rdf_enums.node_type =
  "ml_librdf_node_get_type"

external node_get_uri : node -> uri option = "ml_librdf_node_get_uri"

external node_is_blank : node -> bool = "ml_librdf_node_is_blank"
external node_is_literal : node -> bool = "ml_librdf_node_is_literal"
external node_is_resource : node -> bool = "ml_librdf_node_is_resource"


external pointer_of_node : node -> Nativeint.t = "ml_pointer_of_custom"


let (add_node, incr_node, decr_node) =
  Rdf_misc.create_pointer_counter "node"
  pointer_of_node free_node;;

let new_node world =
  let m = new_node world in
  Rdf_misc.do_opt add_node m;
  m
;;

let new_node_from_literal world ?xml_language ?(is_wf_xml=true) string =
  new_node_from_literal world string xml_language is_wf_xml
;;

let new_node_from_normalised_uri_string world ~uri ~source ~base =
  new_node_from_normalised_uri_string world uri source base
;;

let new_node_from_typed_literal world ?xml_language ?datatype value =
  new_node_from_typed_literal world value xml_language datatype
;;

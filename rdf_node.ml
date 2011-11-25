(** *)

open Rdf_types;;

external new_node : world -> node option = "ml_librdf_new_node"
external free_node : node -> unit = "ml_librdf_free_node"

external new_node_from_blank_identifier : world -> string option -> node option =
  "ml_librdf_new_node_from_identifier"

external new_node_from_literal : world -> string -> string option -> bool -> node option =
  "ml_librdf_new_node_literal"

external new_node_from_normalised_uri_string : world -> string -> uri -> uri -> node option =
  "ml_librdf_new_node_from_normalised_uri_string"

external new_node_from_typed_literal : world -> string -> string option -> uri option -> node option =
  "ml_librdf_new_node_typed_literal"

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

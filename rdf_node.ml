(** *)

open Rdf_types;;

let dbg = Rdf_misc.create_log_fun ~prefix: "Rdf_node" "ORDF_NODE";;

(**/**)
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
    external get_blank_identifier : node -> string = "ml_librdf_node_get_blank_identifier"
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

    external print : node -> out_channel -> unit = "ml_librdf_node_print"

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

let on_new_node fun_name = function
  None -> raise (Node_creation_failed fun_name)
| Some n -> to_finalise n; n
;;

let new_node world = on_new_node "" (Raw.new_node world);;

let copy_node node =
  on_new_node "from_node" (Raw.new_from_node node)
;;

let new_from_blank_identifier ?string world =
  on_new_node "from_blank_identifier"
  (Raw.new_from_blank_identifier world string)
;;

let new_from_literal world ?xml_language ?(is_wf_xml=true) string =
  on_new_node "from_literal"
  (Raw.new_from_literal world string xml_language is_wf_xml)
;;

let new_from_normalised_uri_string world ~uri ~source ~base =
  on_new_node "from_normalised_uri_string"
  (Raw.new_from_normalised_uri_string world uri source base)
;;

let new_from_typed_literal world ?xml_language ?datatype value =
  on_new_node "from_typed_literal"
  (Raw.new_from_typed_literal world value xml_language datatype)
;;

let new_from_uri world uri =
  on_new_node "from_uri"
  (Raw.new_from_uri world uri)
;;

let new_from_uri_local_name world uri name =
  on_new_node "from_uri_local_name"
  (Raw.new_from_uri_local_name world uri name)
;;

let new_from_uri_string world string =
  on_new_node "from_uri_string"
  (Raw.new_from_uri_string world string)
;;

let get_uri node =
  Rdf_misc.map_opt Rdf_uri.copy_uri (Raw.get_uri node)
;;


let get_literal_value_datatype_uri node =
  Rdf_misc.map_opt Rdf_uri.copy_uri
  (Raw.get_literal_value_datatype_uri node)
;;

let print = Raw.print;;


  
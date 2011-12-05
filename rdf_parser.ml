(** *)

open Rdf_types;;

(**/**)
module Raw =
  struct
    external new_parser : world ->
      string option -> string option -> uri option -> parser option = "ml_librdf_new_parser"

    external free_parser : parser -> unit = "ml_librdf_free_parser"

    external parser_parse_into_model : parser -> uri -> uri option -> model -> int =
      "ml_librdf_parser_parse_into_model"

    external pointer_of_parser : parser -> Nativeint.t = "ml_pointer_of_custom"
   end

let parser_to_finalise v = Gc.finalise Raw.free_parser v;;
(**/**)

exception Parser_creation_failed of string;;

let on_new_parser fun_name = function
  None -> raise (Parser_creation_failed fun_name)
| Some n -> parser_to_finalise n; n
;;

let new_parser ?name ?mimetype ?uri world =
  on_new_parser "" (Raw.new_parser world name mimetype uri)
;;

let parser_parse_into_model parser ?base uri model =
  let n = Raw.parser_parse_into_model parser uri base model in
  if n <> 0 then failwith "parser_parse_into_model"
;;
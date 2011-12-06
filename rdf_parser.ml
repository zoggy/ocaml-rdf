(** *)

open Rdf_types;;

let dbg = Rdf_misc.create_log_fun ~prefix: "Rdf_parser" "ORDF_PARSER";;

(**/**)
module Raw =
  struct
    external new_parser : world ->
      string option -> string option -> uri option -> parser option = "ml_librdf_new_parser"

    external free : parser -> unit = "ml_librdf_free_parser"

    external parse_into_model : parser -> uri -> uri option -> model -> int =
      "ml_librdf_parser_parse_into_model"

    external parse_string_into_model : parser -> string -> uri option -> model -> int =
      "ml_librdf_parser_parse_string_into_model"

    external pointer_of_parser : parser -> Nativeint.t = "ml_pointer_of_custom"
   end

let free v =
  dbg (fun () -> Printf.sprintf "Freeing parser %s"
   (Nativeint.to_string (Raw.pointer_of_parser v)));
  Raw.free v
;;
let to_finalise v = Gc.finalise free v;;
(**/**)

exception Parser_creation_failed of string;;

let on_new_parser fun_name = function
  None -> raise (Parser_creation_failed fun_name)
| Some n -> to_finalise n; n
;;

let new_parser ?name ?mimetype ?uri world =
  on_new_parser "" (Raw.new_parser world name mimetype uri)
;;

let parse_into_model parser ?base uri model =
  let n = Raw.parse_into_model parser uri base model in
  if n <> 0 then failwith "parser_parse_into_model"
;;

let parse_into_model parser ?base string model =
  let n = Raw.parse_string_into_model parser string base model in
  if n <> 0 then failwith "parser_parse_string_into_model"
;;
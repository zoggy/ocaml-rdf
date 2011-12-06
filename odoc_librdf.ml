(** Custom ocamldoc generator to produce the ocamlrdf reference
   documentation with links to librdf reference documentation. *)

open Odoc_info


let base_uri = ref "http://librdf.org/docs/api/"
let icon_url = ref "rdf.png"

(*c==v=[String.no_blanks]=1.0====*)
let no_blanks s =
  let len = String.length s in
  let buf = Buffer.create len in
  for i = 0 to len - 1 do
    match s.[i] with
      ' ' | '\n' | '\t' | '\r' -> ()
    | c -> Buffer.add_char buf c
  done;
  Buffer.contents buf
(*/c==v=[String.no_blanks]=1.0====*)

let replace_underscores s =
  let s = String.copy s in
  for i = 0 to String.length s - 1 do
    match s.[i] with
      '_' -> s.[i] <- '-'
    | _ -> ()
  done;
  s
;;

let () = Odoc_info.Args.add_option
  ("--base-uri", Arg.Set_string base_uri,
  "<uri> set base uri; default is "^ !base_uri)
;;

let () = Odoc_info.Args.add_option
  ("--icon-url", Arg.Set_string icon_url,
  "<url> set icon url for links to rdf doc; default is "^ !icon_url)
;;

let rdf_link ?(text="Doc") url =
  Printf.sprintf "<img width=\"16\" src=\"%s\" alt=\"\"/> <a href=\"%s\">%s</a>"
  !icon_url url text

class gen () =
  object (self)
    inherit Odoc_html.html as html
    val mutable rdf_mod_uri = ""
    val mutable rdf_prefix = ""

    method html_of_rdf_mod t =
      let uri =
        match t with
          [] -> ""
        | [Raw s] -> s
        | _ -> ""
      in
      rdf_mod_uri <- uri ;
      let url = !base_uri ^ uri in
      rdf_link url

    method html_of_rdf_prefix t =
      match t with
        [Raw s] -> rdf_prefix <- s; ""
      | _ -> "<b>Bad argument for @rdf-prefix</b>"

    method html_of_rdf t =
      let uri =
        match t with
          [] -> ""
        | [Raw s] -> s
        | _ -> ""
      in
      let url = Printf.sprintf "%s%s#%s%s" !base_uri rdf_mod_uri
        (replace_underscores rdf_prefix)
        (replace_underscores uri)
      in
      let text = rdf_prefix ^ uri in
      rdf_link ~text url

    initializer
      tag_functions <-
      ("rdfmod", self#html_of_rdf_mod) ::
      ("rdfprefix", self#html_of_rdf_prefix) ::
      ("rdf", self#html_of_rdf) ::
       tag_functions
  end;;

let generator = ((new gen ()) :> Odoc_args.doc_generator)

let _ = Odoc_args.set_doc_generator (Some generator)

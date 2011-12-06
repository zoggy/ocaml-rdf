(** Custom ocamldoc generator to produce the ocamlrdf reference
   documentation with links to librdf reference documentation. *)

open Odoc_info


let rdf_base_uri = ref "http://librdf.org/docs/api/"
let raptor_base_uri = ref "http://librdf.org/raptor/api/"
let rasqal_base_uri = ref "http://librdf.org/rasqal/docs/api/"

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
(*c==v=[String.strip_string]=1.0====*)
let strip_string s =
  let len = String.length s in
  let rec iter_first n =
    if n >= len then
      None
    else
      match s.[n] with
        ' ' | '\t' | '\n' | '\r' -> iter_first (n+1)
      | _ -> Some n
  in
  match iter_first 0 with
    None -> ""
  | Some first ->
      let rec iter_last n =
        if n <= first then
          None
        else
          match s.[n] with
            ' ' | '\t' | '\n' | '\r' -> iter_last (n-1)
          |	_ -> Some n
      in
      match iter_last (len-1) with
        None -> String.sub s first 1
      |	Some last -> String.sub s first ((last-first)+1)
(*/c==v=[String.strip_string]=1.0====*)

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
  ("--rdf-uri", Arg.Set_string rdf_base_uri,
  "<uri> set librdf base uri; default is "^ !rdf_base_uri)
;;
let () = Odoc_info.Args.add_option
  ("--raptor-uri", Arg.Set_string raptor_base_uri,
  "<uri> set raptor base uri; default is "^ !raptor_base_uri)
;;
let () = Odoc_info.Args.add_option
  ("--rasqal-uri", Arg.Set_string rasqal_base_uri,
  "<uri> set rasqal base uri; default is "^ !rasqal_base_uri)
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
    val mutable current_base_uri = ""
    val mutable rdf_mod_uri = ""
    val mutable rdf_prefix = ""

    method html_of_rdf_mod t =
      let uri =
        match t with
          [] -> ""
        | [Raw s] -> s
        | _ -> ""
      in
      rdf_mod_uri <- (strip_string uri) ;
      let url = current_base_uri ^ rdf_mod_uri in
      rdf_link url

    method html_of_rdf_prefix t =
      match t with
        [Raw s] -> rdf_prefix <- strip_string s; ""
      | _ -> "<b>Bad argument for @rdf-prefix</b>"

    method html_of_rdf t =
      let uri =
        match t with
          [] -> ""
        | [Raw s] -> s
        | _ -> ""
      in
      let url = Printf.sprintf "%s%s#%s%s"
        current_base_uri
        rdf_mod_uri
        (replace_underscores rdf_prefix)
        (strip_string (replace_underscores uri))
      in
      let text = rdf_prefix ^ uri in
      rdf_link ~text url

    method generate_for_module pre post modu =
      let s = String.lowercase modu.Module.m_name in
      begin
        try
          ignore(Str.search_forward (Str.regexp_string "raptor") s 0);
          current_base_uri <- !raptor_base_uri
        with
          Not_found ->
            try
              ignore(Str.search_forward (Str.regexp_string "rasqal") s 0);
              current_base_uri <- !rasqal_base_uri
            with Not_found ->
                current_base_uri <- !rdf_base_uri
      end;
      html#generate_for_module pre post modu

    method html_of_custom_text b s t =
      match s with
        "rdf" -> Buffer.add_string b (self#html_of_rdf t)
      | "rdfmod" -> Buffer.add_string b (self#html_of_rdf_mod t)
      | "rdfprefix" -> Buffer.add_string b (self#html_of_rdf_prefix t)
      | _ -> html#html_of_custom_text b s t

    initializer
      tag_functions <-
      ("rdfmod", self#html_of_rdf_mod) ::
      ("rdfprefix", self#html_of_rdf_prefix) ::
      ("rdf", self#html_of_rdf) ::
       tag_functions
  end;;

let generator = ((new gen ()) :> Odoc_args.doc_generator)

let _ = Odoc_args.set_doc_generator (Some generator)

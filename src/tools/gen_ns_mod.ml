(*********************************************************************************)
(*                OCaml-RDF                                                      *)
(*                                                                               *)
(*    Copyright (C) 2012-2016 Institut National de Recherche en Informatique     *)
(*    et en Automatique. All rights reserved.                                    *)
(*                                                                               *)
(*    This program is free software; you can redistribute it and/or modify       *)
(*    it under the terms of the GNU Lesser General Public License version        *)
(*    3 as published by the Free Software Foundation.                            *)
(*                                                                               *)
(*    This program is distributed in the hope that it will be useful,            *)
(*    but WITHOUT ANY WARRANTY; without even the implied warranty of             *)
(*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the              *)
(*    GNU General Public License for more details.                               *)
(*                                                                               *)
(*    You should have received a copy of the GNU General Public License          *)
(*    along with this program; if not, write to the Free Software                *)
(*    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA                   *)
(*    02111-1307  USA                                                            *)
(*                                                                               *)
(*    Contact: Maxence.Guesdon@inria.fr                                          *)
(*                                                                               *)
(*********************************************************************************)

(** Generate OCaml code defining IRIs from a RDFs graph. *)


let caml_kw = List.fold_right
  Rdf_types.SSet.add
    [
      "and" ; "as" ; "assert" ; "begin"; "class"; "constraint"; "do";
      "done"; "downto"; "else"; "end"; "exception"; "external";
      "false"; "for"; "fun"; "function"; "functor";
      "if"; "in"; "include"; "inherit"; "initializer";
      "lazy"; "let"; "match"; "method"; "module"; "mutable";
      "new"; "object"; "of"; "open"; "or"; "private";
      "rec"; "sig"; "struct"; "then"; "to"; "true"; "try"; "type";
      "val"; "virtual"; "when"; "while"; "with";
      "mod"; "land"; "lor"; "lxor"; "lsl"; "lsr"; "asr";
    ]
    Rdf_types.SSet.empty
;;


let caml_id ?(protect=false) s typ =
  let typ_prefix =
    if Iri.equal typ Rdf_rdfs.c_Class then
      "c_"
    else if Iri.equal typ Rdf_rdfs.c_Datatype then
        "dt_"
      else
      ""
  in
  let s = Bytes.of_string s in
  let len = Bytes.length s in
  for i = 0 to len - 1 do
    let c = String.get s i in
    match c with
      'a'..'z' | 'A'..'Z' | '0'..'9' | '_' ->
        if typ_prefix = "" && i = 0 then Bytes.set s i (Char.lowercase c)
    | _ -> Bytes.set s i '_'
  done;
  let s = typ_prefix ^ (Bytes.to_string s) in
  if protect && Rdf_types.SSet.mem s caml_kw
  then s^"_"
  else s
;;

let get_properties g =
  let q =
   "PREFIX rdf: <"^(Iri.to_string Rdf_rdf.rdf)^">
    PREFIX rdfs: <"^(Iri.to_string Rdf_rdfs.rdfs)^">
    PREFIX owl: <"^(Iri.to_string Rdf_owl.owl)^">
    SELECT ?prop ?comment ?comment_en ?type
      { ?prop a ?type .
        OPTIONAL { ?prop rdfs:comment ?comment FILTER (!LangMatches(lang(?comment),\"*\")) }
        OPTIONAL { ?prop rdfs:comment ?comment_en FILTER LangMatches(lang(?comment_en),\"en\") }
        FILTER (?type IN (rdf:Property, rdfs:Class, owl:Class, rdfs:Datatype))
      }
      ORDER BY LCASE(STR(?prop))"
  in
  let ds = Rdf_ds.simple_dataset g in
  let q = Rdf_sparql.query_from_string q in
  let sols = Rdf_sparql.select (g.Rdf_graph.name()) ds q in
  let f acc sol =
    match Rdf_sparql.get_iri sol (g.Rdf_graph.name()) "prop" with
      exception Rdf_dt.Error _ ->
        prerr_endline (Printf.sprintf "Ignoring prop %s"
          (Rdf_term.string_of_term (Rdf_sparql.get_term sol "prop")));
        acc
    | prop ->
        let typ =  Rdf_sparql.get_iri sol (g.Rdf_graph.name()) "type" in
        let comment =
          if Rdf_sparql.is_bound sol "comment_en" then
            Some (Rdf_sparql.get_string sol "comment_en")
          else
            if Rdf_sparql.is_bound sol "comment" then
              Some (Rdf_sparql.get_string sol "comment")
            else
              None
        in
        (prop, comment, typ) :: acc
  in
  List.fold_left f [] sols
;;

let get_under s1 s2 =
  let len1 = String.length s1 in
  let len2 = String.length s2 in
  if len2 > len1 && (String.sub s2 0 len1) = s1 then
    String.sub s2 len1 (len2 - len1)
  else
    ""
;;

let gen_impl ?(comments=true) oc prefix base props =
  let p fmt = Printf.fprintf oc fmt in
  let pc fmt =
    if comments then
      Printf.ksprintf (fun s -> p "\n(** %s *)\n" s) fmt
    else
      Printf.ksprintf (fun s -> ()) fmt
  in
  pc "Elements of [%s]" (Iri.to_string base) ;
  p "%s" "\n";
  pc "[%s]" (Iri.to_string base);
  p "let %s_str = \"%s\";;\n" prefix (Iri.to_string base) ;
  p "let %s = Iri.of_string %s_str ;;\n" prefix prefix;
  p "let %s_ s = Iri.of_string (%s_str ^ s);;\n\n" prefix prefix;

  let f (prop, comment, typ) =
    (match comment with None -> () | Some c -> pc "%s" c) ;
    p "let %s = %s_ \"%s\" ;;\n" (caml_id ~protect: true prop typ) prefix prop
  in
  List.iter f props;

  p "%s" "\nmodule Open = struct\n";
  let f (prop, comment, typ) =
    (match comment with None -> () | Some c -> pc "%s" c) ;
    p "  let %s_%s = %s\n" prefix (caml_id prop typ) (caml_id ~protect: true prop typ)
  in
  List.iter f props;
  p "%s" "end\n"
;;

let gen_intf oc prefix base props =
  let p fmt = Printf.fprintf oc fmt in
  let pc ?(margin="") fmt = Printf.ksprintf (fun s -> p "%s(** %s *)\n" margin s) fmt in
  pc "Elements of [%s]" (Iri.to_string base) ;
  p "%s" "\n";
  pc "[%s]" (Iri.to_string base);
  p "val %s : Iri.t\n" prefix ;
  p "val %s_ : string -> Iri.t\n\n" prefix ;

  let f (prop, comment, typ) =
    (match comment with None -> () | Some c -> pc "%s" c) ;
    p "val %s : Iri.t\n\n" (caml_id ~protect: true prop typ)
  in
  List.iter f props;

  p "%s" "\nmodule Open : sig\n" ;
  let f (prop, comment, typ) =
    (match comment with None -> () | Some c -> pc ~margin: "  " "%s" c) ;
    p "  val %s_%s : Iri.t\n\n" prefix (caml_id prop typ)
  in
  List.iter f props;
  p "%s" "end\n";
;;


let generate ?file prefix base g =
  let props = get_properties g in
  let s_base = Iri.to_string base in
  let f acc (prop, comment, typ) =
    let s_prop = Iri.to_string prop in
    match get_under s_base s_prop with
      "" -> acc
    | s -> (s, comment, typ) :: acc
  in
  let props = List.fold_left f [] props in
  match file with
    None -> gen_impl stdout prefix base props
  | Some f ->
      let oc = open_out (f^".ml") in
      gen_impl ~comments: false oc prefix base props;
      close_out oc;

      let oc = open_out (f^".mli") in
      gen_intf oc prefix base props;
      close_out oc;
;;


let fatal s = prerr_endline s ; exit 1 ;;
let usage = Printf.sprintf "Usage: %s [options] <prefix> <base_iri> <file>" Sys.argv.(0);;

let main () =
  let load = ref Rdf_xml.from_file in
  let file_prefix = ref None in
  let read_base = ref None in
  let args = ref [] in
  Arg.parse
    [
      "--ttl", Arg.Unit (fun () -> load := Rdf_ttl.from_file),
      " indicate input file is in turtle format rather than XML/RDF" ;

      "-f", Arg.String (fun s -> file_prefix := Some s),
      " <s> generate code in <s>.ml and <s>.mli" ;

      "-b", Arg.String (fun s -> read_base := Some s),
      " <iri> use <iri> as base used when reading graph, default is <base_iri>" ;
    ]
    (fun s -> args := s :: !args)
    (usage^"\nwhere options are:");
  match List.rev !args with
  | prefix :: base_iri :: file :: _ ->
      begin
        try
          let base_iri = Iri.of_string base_iri in
          let options = [ "storage", "mem" ] in
          let g = Rdf_graph.open_graph ~options base_iri in
          let base = match !read_base with
              None -> base_iri
            | Some s -> Iri.of_string s
          in
          !load g ~base file ;
          generate ?file: !file_prefix prefix base_iri g
        with
          Iri.Error e -> failwith (Iri.string_of_error e)
        | Rdf_ttl.Error e -> failwith (Rdf_ttl.string_of_error e)
        | Rdf_xml.Invalid_rdf msg -> failwith msg
        | Rdf_sparql.Error e -> failwith (Rdf_sparql.string_of_error e)
       (* | Rdf_dt.Error e -> failwith (Rdf_dt.string_of_error e)*)
      end
  | _ -> fatal usage
;;


(*c==v=[Misc.safe_main]=1.0====*)
let safe_main main =
  try main ()
  with
    Failure s
  | Sys_error s ->
      prerr_endline s;
      exit 1
(*/c==v=[Misc.safe_main]=1.0====*)

let () = safe_main main;;
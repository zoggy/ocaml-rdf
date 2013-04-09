(*********************************************************************************)
(*                OCaml-RDF                                                      *)
(*                                                                               *)
(*    Copyright (C) 2012 Institut National de Recherche en Informatique          *)
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

(** Dumping SVG files (through graphviz's dot) from RDF graph. *)

let fatal s = prerr_endline s ; exit 1 ;;
let usage = Printf.sprintf "Usage: %s file.rdf outdir" Sys.argv.(0);;

(*c==v=[Misc.safe_main]=1.0====*)
let safe_main main =
  try main ()
  with
    Failure s
  | Sys_error s ->
      prerr_endline s;
      exit 1
(*/c==v=[Misc.safe_main]=1.0====*)

let main () =
  let (file, outdir) =
    if Array.length Sys.argv < 3 then fatal usage;
    (Sys.argv.(1), Sys.argv.(2))
  in
  let options = [ "storage", "mem" ] in
  let g = Rdf_graph.open_graph ~options (Rdf_uri.uri "http://hello.fr") in
  Rdf_xml.from_file g (Rdf_uri.uri file) file;
  ignore (Sys.command ("mkdir -p "^(Filename.quote outdir)));
  let subjects = g.Rdf_graph.subjects () in
  let href = function
    Rdf_node.Uri uri ->
      prerr_endline ("href for "^(Rdf_uri.string uri));
      let hex = Digest.to_hex (Digest.string (Rdf_uri.string uri)) in
      let svg_file = hex^".svg" in
      if Filename.is_relative outdir then
        Some svg_file
      else
        Some ("file://"^(Filename.concat outdir svg_file))
  | _ -> None
  in
  let f = function
    Rdf_node.Uri uri ->
      let hex = Digest.to_hex (Digest.string (Rdf_uri.string uri)) in
      let dot_file = (Filename.concat outdir hex)^".dot" in
      let svg_file = (Filename.concat outdir hex)^".svg" in
      let dot = Rdf_dot.dot_of_uri g ~href uri in
      let oc = open_out dot_file in
      output_string oc dot;
      close_out oc;
      let com = "dot -Grankdir=TB -Tsvg -o " ^ (Filename.quote svg_file) ^ " " ^ (Filename.quote dot_file) in
      ignore(Sys.command com);
      print_endline ((Rdf_uri.string uri)^ " graph output to " ^ svg_file)
  | _ -> ()
  in
  List.iter f subjects;
;;


let () = safe_main main;;




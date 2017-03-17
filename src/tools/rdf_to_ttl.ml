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

(** Read an RDF graph in XML and output it in turtle format.*)

let fatal s = prerr_endline s ; exit 1 ;;

let base = ref (Iri.of_string "http://base.foo")
let files = ref []
let compact = ref false
let options = [
  "-b", Arg.String (fun s -> base := Iri.of_string s),
  "iri use given iri as base iri" ;

  "--compact", Arg.Set compact,
  " generate compact turtle" ;
  ]
let usage = Printf.sprintf
  "Usage: %s [options] file.rdf [file2.rdf [...]]\nwhere options are:" Sys.argv.(0);;

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
  try
    Arg.parse options (fun s -> files := s :: !files) usage ;
    match List.rev !files with
      [] -> fatal (Arg.usage_string options usage);
    | files ->
        let options = [ "storage", "mem" ] in
        let g = Rdf_graph.open_graph ~options !base in
        List.iter (Rdf_xml.from_file g) files;
        print_endline (Rdf_ttl.to_string ~compact:!compact g)
  with
    e ->
      let msg =
        match e with
          Iri.Error e -> Iri.string_of_error e
        | _ -> Printexc.to_string e
      in
      failwith msg
;;

let () = safe_main main;;

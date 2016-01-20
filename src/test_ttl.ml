(*********************************************************************************)
(*                OCaml-RDF                                                      *)
(*                                                                               *)
(*    Copyright (C) 2012-2014 Institut National de Recherche en Informatique     *)
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

(** Testing Turtle parser. *)

let fatal s = prerr_endline s ; exit 1 ;;
let usage = Printf.sprintf "Usage: %s [options] files.ttl\nwhere options are:" Sys.argv.(0);;

let base = ref None

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
  let args = ref [] in
  Arg.parse
    [ "-b", Arg.String (fun s -> base := Some s), "<url> set base url" ; ]
    (fun s -> args := s :: !args)
    usage;

  let files = List.rev !args in
  let options = [ "storage", "mem" ] in
  let g = Rdf_graph.open_graph ~options (Iri.of_string "http://hello.fr") in
  List.iter
    (fun file ->
       try
         let base = match !base with None -> None | Some s -> Some (Iri.of_string s) in
         Rdf_ttl.from_file g ?base file
       with Rdf_ttl.Error e ->
           prerr_endline ("File "^file^": "^(Rdf_ttl.string_of_error e));
           exit 1
       | Rdf_iri.Invalid_iri (s, msg) ->
           prerr_endline ("Invalid IRI "^s^" : "^msg);
           exit 1
    ) files;
  let dot = Rdf_dot.dot_of_graph g in
  (*print_string dot;*)
  g.Rdf_graph.set_namespaces [];
  print_string (Rdf_ttl.to_string g);
  let oc = open_out "/tmp/foo.dot" in
  output_string oc dot;
  close_out oc
;;


let () = safe_main main;;

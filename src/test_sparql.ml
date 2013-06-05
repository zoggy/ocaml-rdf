(*********************************************************************************)
(*                OCaml-RDF                                                      *)
(*                                                                               *)
(*    Copyright (C) 2012-2013 Institut National de Recherche en Informatique     *)
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

(** Testing Sparql parser. *)

let fatal s = prerr_endline s ; exit 1 ;;
let usage = Printf.sprintf "Usage: %s [options] <queries>\nwhere options are:" Sys.argv.(0);;


(*c==v=[Misc.safe_main]=1.0====*)
let safe_main main =
  try main ()
  with
    Failure s
  | Sys_error s ->
      prerr_endline s;
      exit 1
(*/c==v=[Misc.safe_main]=1.0====*)

let parse_query parse source =
  try
    let q = parse source in
    ignore(q)
  with
    Rdf_sparql.Error e ->
      prerr_endline (Rdf_sparql.string_of_error e);
      exit 1
;;

let parse_query_string = parse_query Rdf_sparql.parse_from_string;;
let parse_query_file = parse_query Rdf_sparql.parse_from_file;;

let files = ref [];;

let main () =
  let args = ref [] in
  Arg.parse
    [
      "-f", Arg.String (fun f -> files := f :: !files),
      "<file> read query from <file>" ;
    ]
    (fun s -> args := s :: !args)
    usage;

  let queries = List.rev !args in
  let files = List.rev !files in
  List.iter parse_query_string queries;
  List.iter parse_query_file files;
;;


let () = safe_main main;;

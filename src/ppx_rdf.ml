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

(** OCaml syntax extension to check syntax of Sparql queries at compile time. *)


module Re = Str
open Ast_mapper
open Ast_helper
open Asttypes
open Parsetree
open Longident
open Location

exception Error of Location.t * string

let () =
  Location.register_error_of_exn (fun exn ->
    match exn with
    | Error (loc, msg) -> Some (error ~loc msg)
    | _ -> None)


let sparql_node = "sparql"
let re_fmt = Re.regexp
  "\\([^%]?\\)%\\([-+0# ]*[0-9]*\\(\\.[0-9]+\\)?[lLn]?\\(\\({term}\\)\\|[!%@,diXxoSsCcFfEeGgBbat]\\)\\)"

let check_query_fmt loc fmt =
  let f s =
    let res =
      match Re.matched_group 4 s with
        "{term}" -> "<http://foo/bar>"
      | "d" | "i" -> "0"
      | "s" -> "string"
      | "S" -> "\"String\""
      | "o" -> "0"
      | "c" -> "c"
      | "C" -> "'C'"
      | "f" -> "0.0"
      | "F" | "E" | "e" | "G" | "g" -> "0.0e-0"
      | "X" -> "ABCD123"
      | "x" -> "abcd123"
      | "B" | "b" -> "true"
      | "a" -> "\"%a\""
      | "t" -> "\"%t\""
      | "!" -> ""
      | "@" -> "@"
      | "%" -> "%"
      | "," -> ""
      | _ -> "%"^(Re.matched_group 2 s)
    in
    (try Re.matched_group 1 s with _ -> "")^res
  in
  let q = Re.global_substitute re_fmt f fmt in
  try ignore(Rdf_sparql.query_from_string q)
  with Rdf_sparql.Error e ->
      let q = Re.global_replace (Re.regexp_string "\n") "\n  " q in
      let msg = Printf.sprintf "Checking syntax of query\n  %s\n%s" q (Rdf_sparql.string_of_error e) in
      raise (Error (loc, msg))

let getenv_mapper argv =
  { default_mapper with
    expr = fun mapper expr ->
      match expr with
      | { pexp_desc = Pexp_constant (Const_string (s, Some id)) } when id = sparql_node ->
          begin
            check_query_fmt expr.pexp_loc s ;
            Exp.apply ~loc: expr.pexp_loc ~attrs: expr.pexp_attributes
              (Exp.ident (Location.mknoloc (Longident.parse "Printf.ksprintf")))
              ["", Exp.ident (Location.mknoloc (Longident.parse "Rdf_sparql.query_from_string")) ;
                "", Exp.constant ~loc: expr.pexp_loc (Const_string (s, None))
              ]
          end
      | { pexp_desc = Pexp_extension ({ txt = id}, PStr [ { pstr_desc = Pstr_eval (e, atts)}  ]) }
            when id = sparql_node ->
          begin
            match
              match e.pexp_desc with
              | Pexp_constant (Const_string (s, _)) -> Some (e.pexp_loc, s, [])
              | Pexp_apply ({ pexp_desc = Pexp_constant (Const_string (s, _)) ; pexp_loc}, args) ->
                  Some (pexp_loc, s, args)
              | _ -> None
            with
              None -> default_mapper.expr mapper expr
            | Some (loc, fmt, args) ->
                check_query_fmt loc fmt ;
                let terms = ref [] in
                let n = ref (-1) in
                let f s =
                  incr n;
                  let g = Re.matched_group 4 s in
                  match g with
                    "{term}" -> terms := !n :: !terms ; (Re.matched_group 1 s)^"%s"
                  | _ -> Re.matched_string s
                in
                let fmt = Re.global_substitute re_fmt f fmt in
                let rec iter i = function
                  [] -> []
                | (l, e) :: q when List.mem i !terms ->
                    let e = Exp.apply
                      (Exp.ident (Location.mknoloc (Longident.parse "Rdf_term.string_of_term")))
                      [ "", e ]
                    in
                    (l, e) :: iter (i+1) q
                | x :: q -> x :: iter (i+1) q
                in
                let args = iter 0 args in
                let args =
                  ("", Exp.ident (Location.mknoloc (Longident.parse "Rdf_sparql.query_from_string"))) ::
                    ("", Exp.constant ~loc (Const_string (fmt, None))) ::
                    args
                in
                Exp.apply ~loc: expr.pexp_loc ~attrs: expr.pexp_attributes
                  (Exp.ident (Location.mknoloc (Longident.parse "Printf.ksprintf")))
                  args
          end
       (* Delegate to the default mapper. *)
      | x -> default_mapper.expr mapper x;
  }

let () = run_main getenv_mapper

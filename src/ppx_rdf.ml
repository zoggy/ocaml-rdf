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

(** OCaml syntax extension to check syntax of Sparql queries at compile time. *)


module Re = Str

open Migrate_parsetree.OCaml_410.Ast
let ocaml_version = Migrate_parsetree.Versions.ocaml_410

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

let lid_sprintf = Location.mknoloc (Longident.parse "Printf.sprintf")
let lid_sparql_error = Location.mknoloc (Longident.parse "Rdf_sparql.Error")
let lid_sparql_parse_error = Location.mknoloc (Longident.parse "Rdf_sparql.Parse_error")

let gen_code ~loc ~attrs fmt args =
  let args =
    (Nolabel, Exp.constant ~loc (Pconst_string (fmt, None))) :: args
  in
  let f =
    let e =
      Exp.apply ~loc (Exp.ident (Location.mknoloc (Longident.parse "Rdf_sparql.query_from_string")))
        [
          Nolabel, Exp.ident(Location.mknoloc (Longident.parse "_q")) ;
        ]
    in
    let case =
      let pat = Pat.construct lid_sparql_error
        (Some (Pat.construct lid_sparql_parse_error
          (Some (Pat.tuple [ Pat.var (Location.mknoloc "eloc") ; Pat.var (Location.mknoloc "msg")]))
         ))
      in
      let e =
        Exp.let_ Nonrecursive
          [ Vb.mk (Pat.var (Location.mknoloc "msg"))
            (Exp.apply (Exp.ident lid_sprintf)
             [ Nolabel, Exp.constant (Pconst_string ("%s\nin %s", None)) ;
               Nolabel, Exp.ident (Location.mknoloc (Longident.parse "msg")) ;
               Nolabel, Exp.ident (Location.mknoloc (Longident.parse "_q")) ;
             ]
            )
          ]
          (Exp.apply (Exp.ident (Location.mknoloc (Longident.parse "raise")))
           [ Nolabel,
             Exp.construct lid_sparql_error
               (Some (Exp.construct lid_sparql_parse_error
                 (Some (Exp.tuple [
                     Exp.ident (Location.mknoloc (Longident.parse  "eloc")) ;
                     Exp.ident (Location.mknoloc (Longident.parse  "msg")) ;
                   ]))
                ))
           ]
          )
      in
      Exp.case pat e
    in
    let body = Exp.try_ ~loc ~attrs e [case] in
    Exp.fun_ Nolabel None (Pat.var (Location.mknoloc "_q")) body
  in
  Exp.apply ~loc ~attrs
    (Exp.ident (Location.mknoloc (Longident.parse "Printf.ksprintf")))
    ((Nolabel, f) :: args)
;;

let getenv_mapper _config _cookies =
  { default_mapper with
    expr = fun mapper expr ->
      match expr with
      | { pexp_desc = Pexp_constant (Pconst_string (s, Some id)) } when id = sparql_node ->
          begin
            check_query_fmt expr.pexp_loc s ;
            gen_code ~loc:  expr.pexp_loc ~attrs: expr.pexp_attributes s []
          end
      | { pexp_desc = Pexp_extension ({ txt = id}, PStr [ { pstr_desc = Pstr_eval (e, atts)}  ]) }
            when id = sparql_node ->
          begin
            match
              match e.pexp_desc with
              | Pexp_constant (Pconst_string (s, _)) -> Some (e.pexp_loc, s, [])
              | Pexp_apply ({ pexp_desc = Pexp_constant (Pconst_string (s, _)) ; pexp_loc}, args) ->
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
                      [ Nolabel, e ]
                    in
                    (l, e) :: iter (i+1) q
                | x :: q -> x :: iter (i+1) q
                in
                let args = iter 0 args in
                gen_code ~loc: expr.pexp_loc ~attrs: expr.pexp_attributes fmt args
          end
       (* Delegate to the default mapper. *)
      | x -> default_mapper.expr mapper x;
  }

let () =
  Migrate_parsetree.Driver.register ~name:"ppx_rdf" ocaml_version getenv_mapper;
  Migrate_parsetree.Driver.run_as_ppx_rewriter ()

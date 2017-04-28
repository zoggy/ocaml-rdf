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

(** *)

(** See {{:https://github.com/solid/web-access-control-spec#individual-resource-acls} Solid WAC} *)

open Rdf_acl.Open

type rights = int

let no_right = 0

let add_read = (lor) 1
let rem_read = (land) (lnot 1)
let has_read r = r land 1 <> 0
let add_write = (lor) 2
let rem_write = (land) (lnot 2)
let has_write r = r land 2 <> 0
let add_append = (lor) 4
let rem_append = (land) (lnot 4)
let has_append r = r land 4 <> 0
let add_control = (lor) 8
let rem_control = (land) (lnot 8)
let has_control r = r land 8 <> 0

let all_rights = add_read (add_write (add_append (add_control no_right)))

let add_rights_of_modes =
  List.fold_left
    (fun acc mode ->
       if Iri.equal mode acl_c_Read then
         add_read acc
       else if Iri.equal mode acl_c_Write then
          add_write acc
         else if Iri.equal mode acl_c_Append then
             add_append acc
           else if Iri.equal mode acl_c_Control then
               add_control acc
             else acc)

let rights_to_string r =
  let b = Buffer.create 4 in
  let p c = Buffer.add_char b c in
  p '[';
  if has_read r then p 'r' ;
  if has_write r then p 'w' ;
  if has_append r then p 'a' ;
  if has_control r then p 'c' ;
  p ']';
  Buffer.contents b

let modes_of_rights r =
  let acc = [] in
  let acc = if has_read r then acl_c_Read :: acc else acc in
  let acc = if has_write r then acl_c_Write :: acc else acc in
  let acc = if has_append r then acl_c_Append :: acc else acc in
  let acc = if has_control r then acl_c_Control :: acc else acc in
  acc


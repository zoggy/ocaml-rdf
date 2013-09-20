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

(** *)

let string_of_opt = function None -> "" | Some s -> s;;
let opt_of_string = function "" -> None | s -> Some s;;
let map_opt f = function None -> None | Some x -> Some (f x);;

let opt_compare f v1 v2 =
  match v1, v2 with
    None, None -> 0
  | None, _ -> -1
  | _, None -> 1
  | Some v1, Some v2 -> f v1 v2
;;

let create_log_fun_with_set ?prefix ?(print=prerr_endline) env_var =
  let log_level =
    ref
      (try int_of_string (Sys.getenv env_var)
      with _ -> 0)
  in
  let pref =
    match prefix with
      None -> ""
    | Some s -> "[" ^ s ^ "]"
  in
  (fun ?loc ?(level=1) f ->
    if !log_level >= level then
       begin
         let loc =
           match loc with
             None -> ""
           | Some s -> "[" ^ s ^ "]"
         in
         let sep = match pref, loc with "", "" -> "" | _ -> " " in
         let s = pref ^ loc ^ sep ^ (f()) in
         print s
       end
  ),
  ((:=) log_level)
;;

let create_log_fun ?prefix ?print env_var =
  fst (create_log_fun_with_set ?prefix ?print env_var);;

let compare_list comp =
  let rec iter l1 l2 =
    match l1, l2 with
      [], [] -> 0
    | [], _ -> -1
    | _, [] -> 1
    | h1 :: q1, h2 :: q2 ->
        match comp h1 h2 with
          0 -> iter q1 q2
        | n -> n
  in
  iter
;;

(*c==v=[File.string_of_file]=1.0====*)
let string_of_file name =
  let chanin = open_in_bin name in
  let len = 1024 in
  let s = String.create len in
  let buf = Buffer.create len in
  let rec iter () =
    try
      let n = input chanin s 0 len in
      if n = 0 then
        ()
      else
        (
         Buffer.add_substring buf s 0 n;
         iter ()
        )
    with
      End_of_file -> ()
  in
  iter ();
  close_in chanin;
  Buffer.contents buf
(*/c==v=[File.string_of_file]=1.0====*)

(*c==v=[String.split_string]=1.1====*)
let split_string ?(keep_empty=false) s chars =
  let len = String.length s in
  let rec iter acc pos =
    if pos >= len then
      match acc with
        "" -> []
      | _ -> [acc]
    else
      if List.mem s.[pos] chars then
        match acc with
          "" ->
            if keep_empty then
              "" :: iter "" (pos + 1)
            else
              iter "" (pos + 1)
        | _ -> acc :: (iter "" (pos + 1))
      else
        iter (Printf.sprintf "%s%c" acc s.[pos]) (pos + 1)
  in
  iter "" 0
(*/c==v=[String.split_string]=1.1====*)

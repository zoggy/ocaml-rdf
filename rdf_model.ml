(*********************************************************************************)
(*                OCaml-RDF                                                      *)
(*                                                                               *)
(*    Copyright (C) 2011 Institut National de Recherche en Informatique          *)
(*    et en Automatique. All rights reserved.                                    *)
(*                                                                               *)
(*    This program is free software; you can redistribute it and/or modify       *)
(*    it under the terms of the GNU Lesser General Public License version        *)
(*    2.1 or later as published by the Free Software Foundation.                 *)
(*                                                                               *)
(*    This program is distributed in the hope that it will be useful,            *)
(*    but WITHOUT ANY WARRANTY; without even the implied warranty of             *)
(*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the              *)
(*    GNU Library General Public License for more details.                       *)
(*                                                                               *)
(*    You should have received a copy of the GNU Lesser General Public           *)
(*    License along with this program; if not, write to the Free Software        *)
(*    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA                   *)
(*    02111-1307  USA                                                            *)
(*                                                                               *)
(*    Contact: Maxence.Guesdon@inria.fr                                          *)
(*                                                                               *)
(*                                                                               *)
(*********************************************************************************)

(** Model (RDF graph).
  @rdfmod redland-model.html
  @rdfprefix librdf_
*)

open Rdf_types

(**/**)
let dbg = Rdf_misc.create_log_fun ~prefix: "Rdf_model" "ORDF_MODEL";;

module Raw =
  struct
    external new_model : world -> storage -> string -> model option = "ml_librdf_new_model"
    external free : model -> unit = "ml_librdf_free_model"
    external new_from_model : model -> model = "ml_librdf_new_model_from_model"

    external add_statement : model -> statement -> int =
      "ml_librdf_model_add_statement"

    external add_statements : model -> statement stream -> int =
      "ml_librdf_model_add_statements"

    external remove_statement : model -> statement -> int =
      "ml_librdf_model_remove_statement"

    external contains_statement : model -> statement -> int =
      "ml_librdf_model_contains_statement"

    external find_statements : model -> statement -> statement stream option =
      "ml_librdf_model_find_statements"

    external find_statements_with_options :
      model -> statement -> node option -> hash option -> statement stream option =
      "ml_librdf_model_find_statements_with_options"

    external context_add_statement : model -> node -> statement -> int =
      "ml_librdf_model_context_add_statement"
    external context_add_statements : model -> node -> statement stream -> int =
      "ml_librdf_model_context_add_statements"
    external context_remove_statement : model -> node -> statement -> int =
      "ml_librdf_model_context_remove_statement"

    external get_sources : model -> node -> node -> node iterator option =
      "ml_librdf_model_get_sources"
    external get_arcs : model -> node -> node -> node iterator option =
      "ml_librdf_model_get_arcs"
    external get_targets : model -> node -> node -> node iterator option =
      "ml_librdf_model_get_targets"

    external write : model -> raptor_iostream -> int =
      "ml_librdf_model_write"

    external find_statements_in_context :
      model -> statement -> node option -> statement stream option =
      "ml_librdf_model_find_statements_in_context"

    external get_contexts : model -> node iterator option =
      "ml_librdf_model_get_contexts"

    external query_execute : model -> query -> query_results option =
      "ml_librdf_model_query_execute"

    external get_feature : model -> uri -> node option =
      "ml_librdf_model_get_feature"

    external set_feature : model -> uri -> node -> int =
      "ml_librdf_model_set_feature"

    external transaction_commit : model -> int = "ml_librdf_model_transaction_commit"
    external transaction_get_handle : model -> 'a = "ml_librdf_model_transaction_get_handle"
    external transaction_rollback : model -> int = "ml_librdf_model_transaction_rollback"
    external transaction_start : model -> int = "ml_librdf_model_transaction_start"
    external transaction_start_with_handle :
      model -> 'a -> int = "ml_librdf_model_transaction_start_with_handle"

    external get_storage : model -> storage option =
      "ml_librdf_model_get_storage"

    external load : model ->
      uri -> string option -> string option -> uri option -> int =
      "ml_librdf_model_load"

    external to_string : model ->
      uri option -> string option -> string option -> uri option -> string option =
      "ml_librdf_model_to_string"

    external pointer_of_model : model -> Nativeint.t = "ml_pointer_of_custom"
end

let free v =
  dbg (fun () -> Printf.sprintf "Freeing model %s"
   (Nativeint.to_string (Raw.pointer_of_model v)));
  Raw.free v
;;

let to_finalise v = Gc.finalise free v;;
(**/**)

exception Model_creation_failed of string;;

let on_new_model fun_name = function
  None -> raise (Model_creation_failed fun_name)
| Some n -> to_finalise n; n
;;

(** @rdf new_model *)
let new_model ?(options="") world storage =
  on_new_model "" (Raw.new_model world storage options)
;;

(** @rdf model_add_statement *)
let add_statement model statement =
  let n = Raw.add_statement model statement in
  if n <> 0 then failwith "model_add_statement"
;;

(** @rdf model_add_statement
*)
let add_statements model ?context stream =
  let n =
    match context with
      None -> Raw.add_statements model stream
    | Some node -> Raw.context_add_statements model node stream
  in
  if n <> 0 then
   failwith "model_add_statements"
;;

(** @rdf model_remove_statement *)
let remove_statement model ?context statement =
  let n =
    match context with
     None -> Raw.remove_statement model statement
   | Some node -> Raw.context_remove_statement model node statement
  in
  if n <> 0 then
   failwith "model_remove_statement"
;;

(** @rdf model_contains_statement *)
let contains_statement model statement =
  let n = Raw.contains_statement model statement in
  (*prerr_endline (Printf.sprintf "contains_statement returns %d" n);*)
  if n > 0 then raise Rdf_storage.Illegal_statement;
  (n <> 0)
;;

(** @rdf model_find_statements
*)
let find_statements model ?context ?hash statement =
  match context, hash with
    Some _, None ->
      Rdf_stream.on_new_stream "model_find_statements_in_context"
        (Raw.find_statements_in_context model statement context)
  | None, None ->
      let stream = Raw.find_statements model statement in
      Rdf_stream.on_new_stream "model_find_statements" stream
  | _ ->
      Rdf_stream.on_new_stream "model_find_statements_with_options"
         (Raw.find_statements_with_options
            model statement context hash)
;;

(** @rdf model_get_sources *)
let get_sources model ~arc ~target =
  Rdf_iterator.on_new_iterator "model_get_sources"
    (Raw.get_sources model arc target)
;;

(** @rdf model_get_arcs *)
let get_arcs model ~source ~target =
  Rdf_iterator.on_new_iterator "model_get_arcs"
    (Raw.get_arcs model source target)
;;

(** @rdf model_get_targets *)
let get_targets model ~source ~arc =
  Rdf_iterator.on_new_iterator "model_get_targets"
    (Raw.get_targets model source arc)
;;

(** @rdf model_write *)
let write model iostream =
  let n = Raw.write model iostream in
  if n <> 0 then failwith "model_write"
;;

(** @rdf model_get_contexts *)
let get_contexts model =
  Rdf_iterator.on_new_iterator "model_get_contexts"
    (Raw.get_contexts model)
;;

(** @rdf model_query_execute *)
let query_execute model query =
  Rdf_query_results.on_new_query_results "model_query_execute"
    (Raw.query_execute model query)
;;

(** @rdf model_get_feature *)
let get_feature model uri =
  match Raw.get_feature model uri with
    None -> None
  | n -> Some (Rdf_node.on_new_node "" n)
;;

(** @rdf model_set_feature *)
let set_feature model uri value =
  let n = Raw.set_feature model uri value in
  if n < 0 then raise (Rdf_storage.No_such_feature uri);
  if n > 0 then failwith "model_set_feature"
;;

(** @rdf model_transaction_commit *)
let transaction_commit model =
  let n = Raw.transaction_commit model in
  if n <> 0 then failwith "model_transaction_commit"
;;

(** @rdf model_transaction_get_handle *)
let transaction_get_handle model =
  Raw.transaction_get_handle model
;;

(** @rdf model_transaction_rollback *)
let transaction_rollback model =
  let n = Raw.transaction_rollback model in
  if n <> 0 then failwith "model_transaction_rollback"
;;

(** @rdf model_transaction_start *)
let transaction_start model =
  let n = Raw.transaction_start model in
  if n <> 0 then failwith "model_transaction_start"
;;

(** @rdf model_transaction_start_with_handle *)
let transaction_start_with_handle model h =
  let n = Raw.transaction_start_with_handle model h in
  if n <> 0 then failwith "model_transaction_start_with_handle"
;;

(** @rdf model_load *)
let load model ?name ?mimetype ?typ uri =
  let n = Raw.load model uri name mimetype typ in
  if n <> 0 then failwith "model_load"
;;

(** @rdf model_get_storage *)
let get_storage = Raw.get_storage;;

(** @rdf model_to_string *)
let to_string ?name ?mimetype ?typ ?uri model =
  Raw.to_string model uri name mimetype typ
;;


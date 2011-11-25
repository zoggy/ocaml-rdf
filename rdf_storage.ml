(** *)

open Rdf_types;;

external new_storage : world ->
  string -> string -> string -> storage option = "ml_librdf_new_storage"
external free_storage : storage -> unit = "ml_librdf_free_storage"

external pointer_of_storage : storage -> Nativeint.t = "ml_pointer_of_custom"

let (add_storage, incr_storage, decr_storage) =
  Rdf_misc.create_pointer_counter "storage"
  pointer_of_storage free_storage;;

let new_storage ?(options="") world ~factory ~name =
  let m = new_storage world factory name options in
  Rdf_misc.do_opt add_storage m;
  m
;;

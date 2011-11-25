(** *)

open Rdf_types;;

(**/**)
module Raw =
  struct
    external new_storage : world ->
      string -> string -> string -> storage option = "ml_librdf_new_storage"
    external free_storage : storage -> unit = "ml_librdf_free_storage"

    external pointer_of_storage : storage -> Nativeint.t = "ml_pointer_of_custom"
   end

let storage_to_finalise v = Gc.finalise Raw.free_storage v;;
(**/**)

exception Storage_creation_failed of string;;

let on_new_storage fun_name = function
  None -> raise (Storage_creation_failed fun_name)
| Some n -> storage_to_finalise n; n
;;

let new_storage ?(options="") world ~factory ~name =
  on_new_storage "" (Raw.new_storage world factory name options)
;;


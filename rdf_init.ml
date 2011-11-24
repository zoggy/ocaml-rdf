(** *)

type world

external new_world : unit -> world = "ml_librdf_new_world"
external free_world : world -> unit = "ml_librdf_free_world"
external world_open : world -> unit = "ml_librdf_world_open"
external world_set_rasqal : world -> Rdf_rasqal.world -> unit = "ml_librdf_wprld_set_rasqal"

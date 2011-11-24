(** Example *)

let rasqal = Rdf_rasqal.new_world () ;;
let _ =
  let world = Rdf_init.new_world () in
  Rdf_init.world_set_rasqal world rasqal;
  ();;
for i = 0 to 100000 do ignore(Unix.stat "/tmp")done;;
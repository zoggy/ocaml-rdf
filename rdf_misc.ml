(** *)

module Map = Map.Make (Nativeint);;

let create_pointer_counter id pointer_of free =
  let map = ref Map.empty in
  let decr v =
    prerr_endline ("Removing one "^id);
    let n = pointer_of v in
    try
      let cpt = Map.find n !map in
      let cpt = cpt - 1 in
      if cpt = 0 then
        (map := Map.remove n !map;
         prerr_endline ("freeing "^id);
         free v
        )
      else
        map := Map.add n cpt !map
    with Not_found -> ()
  in
  let incr v =
    prerr_endline ("Adding one "^id);
    let n = pointer_of v in
    begin
      try let c = Map.find n !map in map := Map.add n (c+1) !map
      with Not_found -> map := Map.add n 1 !map
    end
  in
  let add v = incr v ; Gc.finalise decr v in
  (add, incr, decr)
;;

let do_opt f = function None -> ()| Some x -> f x;;

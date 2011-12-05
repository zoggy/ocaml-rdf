(** *)

module Map = Map.Make (Nativeint);;

let create_pointer_counter id pointer_of free =
  let map = ref Map.empty in
  let decr v =
    let n = pointer_of v in
    prerr_endline (Printf.sprintf "Removing one %s: %s" id (Nativeint.to_string n));
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
    let n = pointer_of v in
    prerr_endline
    (Printf.sprintf "Adding one %s: %s" id (Nativeint.to_string n));
    begin
      try let c = Map.find n !map in map := Map.add n (c+1) !map
      with Not_found -> map := Map.add n 1 !map
    end
  in
  let add v = incr v ; Gc.finalise decr v in
  (add, incr, decr)
;;

let do_opt f = function None -> ()| Some x -> f x;;
let map_opt f = function None -> None | Some x -> Some (f x);;

let create_log_fun_with_set ?prefix ?(print=prerr_endline) env_var =
  let log_level =
    ref
      (try int_of_string (Sys.getenv env_var)
      with _ -> 0)
  in
  let pref =
    match prefix with
      None -> ""
    | Some s -> Printf.sprintf "[%s]" s
  in
  (fun ?loc ?(level=1) f ->
    if !log_level >= level then
       begin
         let loc =
           match loc with
             None -> ""
           | Some s -> Printf.sprintf "[%s]" s
         in
         let sep = match pref, loc with "", "" -> "" | _ -> " " in
         let s = Printf.sprintf "%s%s%s%s"
           pref loc sep (f())
         in
         print s
       end
  ),
  ((:=) log_level)
;;

let create_log_fun ?prefix ?print env_var =
  fst (create_log_fun_with_set ?prefix ?print env_var);;

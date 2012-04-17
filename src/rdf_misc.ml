(** *)

let string_of_opt = function None -> "" | Some s -> s;;
let opt_of_string = function "" -> None | s -> Some s;;
let map_opt f = function None -> None | Some x -> Some (f x);;
(** *)

type value =
  | String of string
  | Int of int
  | Float of float
  | Bool of bool
  | Datetime of Netdate.t
  | Ltrl of string * string option (* optional language *)

exception Type_error of value * string

let date_fmt = "%d %b %Y %T %z"

let string v =
  let s =
    match v with
    | String s -> s
    | Int n -> string_of_int n
    | Float f -> string_of_float f
    | Bool true -> "true"
    | Bool false -> "false"
    | Datetime t -> Netdate.format ~fmt: date_fmt t
    | Ltrl (s, _) -> s
  in
  String s

let int v =
  try
    let n =
    match v with
      | String s
      | Ltrl (s, _) -> int_of_string s
      | Int n -> n
      | Float f -> truncate f
      | Bool true -> 1
      | Bool false -> 0
      | Datetime _ -> failwith ""
    in
    Int n
  with
    _ -> raise (Type_error (v, "int"))
;;

let float v =
  try
    let f =
      match v with
      | String s
      | Ltrl (s, _) -> float_of_string s
      | Int n -> float n
      | Float f -> f
      | Bool true -> 1.0
      | Bool false -> 0.0
      | Datetime _ -> failwith ""
    in
    Float f
  with
    _ -> raise (Type_error (v, "float"))
;;


let bool v =
  try
    let b =
      match v with
      | String s
      | Ltrl (s, _) ->
          (match s with
            "1" | "true" -> true
           | "0" | "false" -> false
           | _ -> failwith ""
          )
      | Int n -> n <> 0
      | Float f ->
          (match classify_float f with
             FP_zero | FP_nan -> false
            |_ -> true
          )
      | Bool b -> b
      | Datetime _ -> failwith ""
    in
    Bool b
  with
    _ -> raise (Type_error (v, "bool"))
;;

let datetime v =
  try
    let t =
      match v with
      | String s
      | Ltrl (s, _) -> Netdate.parse s
      | Int _ | Float _ | Bool _  -> failwith ""
      | Datetime t -> t
    in
    Datetime t
  with
    _ -> raise (Type_error (v, "datetime"))
;;

let ltrl v =
  try
    let (s, lang) =
      match v with
      | String s -> (s, None)
      | Ltrl (s, l) -> (s, l)
      | Int n -> (string_of_int n, None)
      | Float f -> (string_of_float f, None)
      | Bool true -> ("true", None)
      | Bool false -> ("false", None)
      | Datetime t -> (Netdate.format ~fmt: date_fmt t, None)
    in
    Ltrl (s, lang)
  with
    _ -> raise (Type_error (v, "ltrl"))
;;


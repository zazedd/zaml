open Common

type level = int

let generic_level = 100000000

(* marking a node, to check for cycles without occur_check *)
let marked_level = -1

type typ =
  | TUnit
  | TInt
  | TBool
  | TVar of tvar ref
  | TArrow of typ * typ * levels

and tvar = Unbound of variable * level | Link of typ
and levels = { mutable new_level : level; mutable old_level : level }

module Tbl = Hashtbl.Make (String)

(* In: "'a" in bytes
   Out: "'b" in bytes *)
let next_letter s =
  let c = Bytes.get s 1 in
  Char.code c + 1 |> Char.chr |> Bytes.set s 1

let rec is_paren = function
  | TVar { contents = Link t } -> is_paren t
  | TArrow (_, _, _) -> true
  | _ -> false

let string_of_typ t =
  let rec go current tbl = function
    | TUnit -> "unit"
    | TBool -> "bool"
    | TInt -> "int"
    | TVar { contents = Link t; _ } -> go current tbl t
    | TVar { contents = Unbound (n, _); _ } -> (
        match Tbl.find_opt tbl n with
        | Some s -> s
        | None ->
            let s = Bytes.to_string current in
            Tbl.add tbl n s;
            next_letter current;
            s)
    | TArrow (a, b, _) ->
        let a_str = go current tbl a in
        let b_str = go current tbl b in
        if is_paren a then "(" ^ a_str ^ ") -> " ^ b_str
        else a_str ^ " -> " ^ b_str
  in
  go (Bytes.of_string "'a") (Tbl.create 1) t

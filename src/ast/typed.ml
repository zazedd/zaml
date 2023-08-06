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

let rec is_paren = function
  | TVar { contents = Link t } -> is_paren t
  | TArrow (_, _, _) -> true
  | _ -> false

let rec string_of_typ = function
  | TUnit -> "unit"
  | TBool -> "bool"
  | TInt -> "int"
  | TVar { contents = Link t; _ } -> string_of_typ t
  | TVar { contents = Unbound (name, _); _ } -> name
  | TArrow (a, b, _) ->
      let a_str = string_of_typ a in
      let b_str = string_of_typ b in
      if is_paren a then "(" ^ a_str ^ ") -> " ^ b_str
      else a_str ^ " -> " ^ b_str

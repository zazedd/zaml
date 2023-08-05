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

let rec string_of_typ = function
  | TUnit -> "unit"
  | TBool -> "bool"
  | TInt -> "int"
  | TArrow (t1, t2, { new_level; old_level }) ->
      Format.sprintf "%s -> %s (%s %s)" (string_of_typ t1) (string_of_typ t2)
        (string_of_int new_level) (string_of_int old_level)
  | TVar { contents = Link t; _ } -> "link" ^ string_of_typ t
  | TVar { contents = Unbound (name, l); _ } ->
      name ^ "level = " ^ string_of_int l

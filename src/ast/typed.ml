open Common

type level = int

let generic_level = 100000000

(* marking a node, to check for cycles without occur_check *)
let marked_level = -1

type typ =
  | TUnit
  | TInt
  | TChar
  | TString
  | TBool
  | TList of typ
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

let rec string_of_typ t =
  let rec go current tbl = function
    | TUnit -> "unit"
    | TInt -> "int"
    | TChar -> "char"
    | TString -> "string"
    | TBool -> "bool"
    | TList t -> string_of_typ t ^ " list"
    | TVar { contents = Link t } -> go current tbl t
    | TVar { contents = Unbound (n, _) } -> (
        match Tbl.find_opt tbl n with
        | Some s -> s
        | None ->
            let s = Bytes.to_string current in
            Tbl.add tbl n s;
            next_letter current;
            s)
    | TArrow (t1, t2, _) ->
        let t1_str = go current tbl t1 in
        let t2_str = go current tbl t2 in
        if is_paren t1 then "(" ^ t1_str ^ ") -> " ^ t2_str
        else t1_str ^ " -> " ^ t2_str
  in
  go (Bytes.of_string "'a") (Tbl.create 1) t

let rec string_of_typ2 t =
  let rec go current tbl = function
    | TUnit -> "unit"
    | TInt -> "int"
    | TChar -> "char"
    | TString -> "string"
    | TBool -> "bool"
    | TList t -> string_of_typ2 t ^ " list"
    | TVar { contents = Link t } -> "TVar link : " ^ go current tbl t
    | TVar { contents = Unbound (n, l) } -> (
        match Tbl.find_opt tbl n with
        | Some s -> Format.sprintf "TVar Unbound (level %d) : %s" l s
        | None ->
            let s = Bytes.to_string current in
            Tbl.add tbl n s;
            next_letter current;
            Format.sprintf "TVar Unbound (level %d) : %s" l s)
    | TArrow (t1, t2, l) ->
        let t1_str = go current tbl t1 in
        let t2_str = go current tbl t2 in
        Format.sprintf "TArrow (level new : %d | old : %d) (%s, %s)" l.new_level
          l.old_level t1_str t2_str
  in
  go (Bytes.of_string "'a") (Tbl.create 1) t

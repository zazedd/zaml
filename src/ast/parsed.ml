open Common

type op =
  | Add
  | Subt
  | Mult
  | Div
  | Mod
  | Cons
  | Concat
  | Eq
  | Ineq
  | Lt
  | Lte
  | Bt
  | Bte

type t =
  | Const of vals
  | Var of variable
  | If of expr * expr * expr
  | Bop of op * expr * expr
  | Let of { name : variable; binding : expr; in_body : expr option }
  | Lambda of { vars : variable list; body : expr }
  | App of expr * expr list

and vals =
  | Unit
  | Int of int
  | Char of char
  | String of string
  | Bool of bool
  | List of expr list

and expr = { expr : t; pos : pos }

let get_expr e = e.expr

let get_let_name = function
  | Let { name; _ } -> "val " ^ name ^ " : "
  | _ -> "- : "

let position
    ((starts, ends) :
      (Lexing.position * Lexing.position) * (Lexing.position * Lexing.position))
    =
  let starts = fst starts in
  let ends = fst ends in
  {
    fname = starts.pos_fname;
    lnum = starts.pos_lnum;
    start = starts.pos_cnum + 1;
    ending = ends.pos_cnum + 1;
  }

let rec string_of_ast = function
  | { expr = Const c; _ } -> string_of_const c
  | { expr = Var s; _ } -> "var name : " ^ s
  | { expr = Let { name; binding; in_body }; _ } -> (
      "let : name : " ^ name ^ "\n" ^ string_of_ast binding
      ^ match in_body with None -> "" | Some b -> string_of_ast b)
  | { expr = Lambda { vars; body }; _ } ->
      List.fold_left (fun acc a -> acc ^ " " ^ a) "Lambda :\nvars : \n" vars
      ^ "\nbody : " ^ string_of_ast body
  | { expr = App (e1, e2); _ } ->
      "App : " ^ string_of_ast e1 ^ " and "
      ^ List.fold_left (fun acc a -> acc ^ string_of_ast a) "" e2
  | _ -> assert false

and string_of_const = function
  | Unit -> "unit"
  | Int i -> "int : " ^ string_of_int i
  | Char c -> "char : '" ^ String.make 1 c ^ "'"
  | String s -> "string : " ^ s
  | Bool b -> "bool : " ^ string_of_bool b
  | List l ->
      "list : "
      ^ List.fold_left (fun acc a -> acc ^ string_of_ast a ^ "; ") "[" l
      ^ "]"

open Common

type t =
  | Unit
  | Int of int
  | Bool of bool
  | Var of variable
  | If of expr * expr * expr
  | Let of { name : variable; binding : expr; in_body : expr option }
  | Lambda of { vars : variable list; body : expr }
  | Fun of {
      name : variable;
      vars : variable list;
      binding : expr;
      in_body : expr option;
    }
  | App of expr * expr

and expr = { expr : t; pos : pos }

let get_expr e = e.expr

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
  | { expr = Unit; _ } -> "unit"
  | { expr = Int i; _ } -> "int : " ^ string_of_int i
  | { expr = Bool b; _ } -> "bool : " ^ string_of_bool b
  | { expr = Var s; _ } -> "var name : " ^ s
  | { expr = Let { name; binding; in_body }; _ } -> (
      "let : name : " ^ name ^ "\n" ^ string_of_ast binding
      ^ match in_body with None -> "" | Some b -> string_of_ast b)
  | { expr = Lambda { vars; body }; _ } ->
      List.fold_left (fun acc a -> acc ^ " " ^ a) "Lambda :\nvars : \n" vars
      ^ "\nbody : " ^ string_of_ast body
  | { expr = Fun { name; vars; binding; in_body }; _ } -> (
      "Fun name : " ^ name
      ^ List.fold_left (fun acc a -> acc ^ " " ^ a) "\nvars : \n" vars
      ^ "\nbinding : " ^ string_of_ast binding ^ "\nbody : "
      ^ match in_body with None -> "" | Some b -> string_of_ast b)
  | { expr = App (e1, e2); _ } ->
      "App : " ^ string_of_ast e1 ^ " and " ^ string_of_ast e2
  | _ -> assert false

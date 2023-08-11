open Ast.Parsed
open Ast.Typed
open Env
open Errors

let is_value = function
  | VUnit | VInt _ | VChar _ | VString _ | VBool _ | VList _ -> true
  | Closure _ -> false

let is_function = function Closure _ -> true | _ -> false

let rec string_of_val = function
  | VUnit -> "()"
  | VInt i -> string_of_int i
  | VChar c -> "'" ^ String.make 1 c ^ "'"
  | VString s -> "\"" ^ s ^ "\""
  | VBool b -> string_of_bool b
  | VList l -> "[" ^ String.concat "; " (List.map string_of_val l) ^ "]"
  | Closure _ -> not_a_value ()

let print_eval e t v =
  get_let_name (get_expr e) ^ string_of_typ t |> print_string;
  let str =
    if is_value v then " = " ^ string_of_val v
    else if is_function v then " = <fun>"
    else ""
  in
  print_endline str

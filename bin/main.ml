open Ast.Parsed
open Ast.Typed
open Parsing
open Typing.Common
open Typing.Errors
open Typing.Env
open Typing.Typecheck

let parse s =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

(* let rec string_of_ast = function *)
(*   | Unit -> "unit" *)
(*   | Int i -> "int = " ^ string_of_int i *)
(*   | Bool b -> "bool = " ^ string_of_bool b *)
(*   | Var s -> "var name = " ^ s *)
(*   | Let { name; binding; in_body } -> ( *)
(*       "let = name = " ^ name ^ "\n" ^ string_of_ast binding *)
(*       ^ match in_body with None -> "" | Some b -> string_of_ast b) *)
(*   | Lambdabda { vars; body } -> *)
(*       List.fold_left (fun acc a -> acc ^ " " ^ a) "Lambdabda =\nvars = \n" vars *)
(*       ^ "\nbody = " ^ string_of_ast body *)
(*   | Fun { name; vars; binding; in_body } -> ( *)
(*       "Fun name = " ^ name *)
(*       ^ List.fold_left (fun acc a -> acc ^ " " ^ a) "\nvars = \n" vars *)
(*       ^ "\nbinding = " ^ string_of_ast binding ^ "\nbody = " *)
(*       ^ match in_body with None -> "" | Some b -> string_of_ast b) *)
(*   | App (e1, e2) -> "App = " ^ string_of_ast e1 ^ " and " ^ string_of_ast e2 *)

let bold_string str = Format.sprintf "\027[1m%s\027[0m" str
let blue_string str = Format.sprintf "\027[34m%s\027[0m" str

let top_type_check exp =
  reset_type_variables ();
  reset_level_adjustment ();
  let ty = typeof Ctx.empty exp in
  cycle_free ty;
  string_of_typ ty |> print_endline;
  ty

let id = Lambda { vars = [ "x" ]; body = Var "x" }

let () =
  assert (
    TArrow
      ( TVar { contents = Unbound ("'a", 1) },
        TVar { contents = Unbound ("'a", 1) },
        { old_level = 1; new_level = 1 } )
    = top_type_check id)

let rec loop () =
  bold_string "zaml" |> print_string;
  blue_string " # " |> print_string;
  let ast = read_line () |> parse |> List.hd in
  let () =
    try top_type_check ast |> ignore
    with TypeError e -> print_endline ("Error: " ^ e)
  in
  loop ()

let () = loop ()

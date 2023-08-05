open Ast.Typed
open Parsing.Parse
open Typing.Env
open Typing.Typecheck

let read_file file =
  try
    let ic = open_in file in
    let len = in_channel_length ic in
    let str = really_input_string ic len in
    close_in ic;
    str
  with
  | Sys_error e ->
      print_endline ("Error: " ^ e);
      exit 1
  | e -> raise e

(* let rec string_of_ast = function *)
(*   | Unit -> "unit" *)
(*   | Int i -> "int : " ^ string_of_int i *)
(*   | Bool b -> "bool : " ^ string_of_bool b *)
(*   | Var s -> "var name : " ^ s *)
(*   | Let { name; binding; in_body } -> ( *)
(*       "let : name : " ^ name ^ "\n" ^ string_of_ast binding *)
(*       ^ match in_body with None -> "" | Some b -> string_of_ast b) *)
(*   | Lambda { vars; body } -> *)
(*       List.fold_left (fun acc a -> acc ^ " " ^ a) "Lambda :\nvars : \n" vars *)
(*       ^ "\nbody : " ^ string_of_ast body *)
(*   | Fun { name; vars; binding; in_body } -> ( *)
(*       "Fun name : " ^ name *)
(*       ^ List.fold_left (fun acc a -> acc ^ " " ^ a) "\nvars : \n" vars *)
(*       ^ "\nbinding : " ^ string_of_ast binding ^ "\nbody : " *)
(*       ^ match in_body with None -> "" | Some b -> string_of_ast b) *)
(*   | App (e1, e2) -> "App : " ^ string_of_ast e1 ^ " and " ^ string_of_ast e2 *)
(*   | _ -> assert false *)

let run file =
  let lines = read_file file in
  let ast_list = lines |> parse in
  List.fold_left
    (fun acc a ->
      let t, t_ctx = type_check acc a in
      string_of_typ t |> print_endline;
      Ctx.merge (fun _ _ x -> x) acc t_ctx)
    Ctx.empty ast_list

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

let run file =
  let lines = read_file file in
  let ast_list = lines |> parse in
  List.fold_left
    (fun acc a ->
      let t, t_ctx = type_check acc a in
      "- : " ^ string_of_typ t |> print_endline;
      Ctx.merge (fun _ _ x -> x) acc t_ctx)
    Ctx.empty ast_list

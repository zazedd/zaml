open Ast.Typed
open Typing.Env
open Typing.Errors
open Typing.Typecheck
open Evaluating.Env
open Evaluating.Eval
open Parsing.Parse
open Parsing.Errors

let bold_string str = Format.sprintf "\027[1m%s\027[0m" str
let blue_string str = Format.sprintf "\027[34m%s\027[0m" str

let exit_repl str =
  str |> bold_string |> print_endline;
  exit 0

let rec run t_ctx e_ctx =
  bold_string "zaml" |> print_string;
  blue_string " # " |> print_string;
  try
    match read_line () with
    | str when str = "#quit" || str = "#exit" ->
        exit_repl "See you later cowboy..."
    | str ->
        let ast = str |> from_string |> parse in
        let t_ctx =
          (List.fold_left (fun acc a ->
               let t', t_ctx' = type_check acc a in
               "- : " ^ string_of_typ t' |> print_string;
               Ctx.merge (fun _ _ x -> x) acc t_ctx'))
            t_ctx ast
        in
        let e_ctx =
          (List.fold_left (fun acc a ->
               let e', e_ctx' = value_of acc a in
               let str = if is_value e' then " = " ^ string_of_val e' else "" in
               print_endline str;
               ECtx.merge (fun _ _ x -> x) acc e_ctx'))
            e_ctx ast
        in
        run t_ctx e_ctx
  with
  | End_of_file -> exit_repl "\nSee you later cowboy..."
  | TypeError e | OccurCheck e | RuntimeError e ->
      print_endline ("Error: " ^ e);
      run t_ctx e_ctx
  | ParsingError e | LexingError e ->
      print_endline e;
      run t_ctx e_ctx

open Typing.Env
open Typing.Errors
open Typing.Typecheck
open Evaluating.Env
open Evaluating.Eval
open Evaluating.Errors
open Evaluating.Common
open Parsing.Parse
open Parsing.Errors

let bold_string str = Format.sprintf "\027[1m%s\027[0m" str
let blue_string str = Format.sprintf "\027[34m%s\027[0m" str

let exit_repl str =
  str |> bold_string |> print_endline;
  exit 0

let rec run ctx =
  bold_string "zaml" |> print_string;
  blue_string " # " |> print_string;
  try
    match read_line () with
    | str when str = "#quit" || str = "#exit" ->
        exit_repl "See you later cowboy..."
    | str ->
        let ast = str |> from_string |> parse in
        let t_ctx, e_ctx =
          List.fold_left
            (fun (acc1, acc2) a ->
              let t', t_ctx' = type_check acc1 a in
              let e', e_ctx' = value_of acc2 a in
              print_eval a t' e';
              ( Ctx.merge (fun _ _ x -> x) acc1 t_ctx',
                ECtx.merge (fun _ _ x -> x) acc2 e_ctx' ))
            ctx ast
        in
        run (t_ctx, e_ctx)
  with
  | End_of_file -> exit_repl "\nSee you later cowboy..."
  | TypeError e | OccurCheck e ->
      print_endline ("Error: " ^ e);
      run ctx
  | RuntimeError e ->
      print_endline ("\nError: " ^ e);
      run ctx
  | ParsingError e | LexingError e ->
      print_endline e;
      run ctx

open Ast.Typed
open Typing.Env
open Typing.Errors
open Typing.Typecheck
open Parsing.Parse

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
        let ast = str |> parse in
        let t_ctx =
          (List.fold_left (fun acc a ->
               let t', t_ctx' = a |> type_check acc in
               "- : " ^ string_of_typ t' |> print_endline;
               Ctx.merge (fun _ _ x -> x) acc t_ctx'))
            ctx ast
        in
        run t_ctx
  with
  | End_of_file -> exit_repl "\nSee you later cowboy..."
  | TypeError e | OccurCheck e ->
      print_endline ("Error: " ^ e);
      run ctx

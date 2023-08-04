open Ast.Typed
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
        let t, t_ctx = str |> parse |> List.hd |> type_check ctx in
        t |> string_of_typ |> print_endline;
        run t_ctx
  with
  | End_of_file -> exit_repl "\nSee you later cowboy..."
  | TypeError e | OccurCheck e ->
      print_endline ("Error: " ^ e);
      run ctx

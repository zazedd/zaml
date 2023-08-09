open Ast.Typed
open Parsing.Parse
open Parsing.Errors
open Typing.Env
open Typing.Typecheck

let run file =
  let lines = from_file file in
  try
    let ast_list = lines |> parse in
    List.fold_left
      (fun acc a ->
        let t, t_ctx = type_check acc a in
        "- : " ^ string_of_typ t |> print_endline;
        Ctx.merge (fun _ _ x -> x) acc t_ctx)
      Ctx.empty ast_list
  with LexingError e | ParsingError e ->
    print_endline e;
    exit 1

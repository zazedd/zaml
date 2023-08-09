open Ast.Typed
open Parsing.Parse
open Parsing.Errors
open Typing.Env
open Typing.Typecheck
open Evaluating.Env
open Evaluating.Eval

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
    |> ignore;
    List.fold_left
      (fun acc a ->
        let e', e_ctx' = value_of acc a in
        let str = if is_value e' then " = " ^ string_of_val e' else "" in
        print_endline str;
        ECtx.merge (fun _ _ x -> x) acc e_ctx')
      ECtx.empty ast_list
    |> ignore
  with LexingError e | ParsingError e ->
    print_endline e;
    exit 1

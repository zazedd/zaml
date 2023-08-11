open Parsing.Parse
open Parsing.Errors
open Typing.Env
open Typing.Typecheck
open Evaluating.Env
open Evaluating.Eval
open Evaluating.Common

let run file ctx =
  let lines = from_file file in
  try
    let ast_list = lines |> parse in
    List.fold_left
      (fun (acc1, acc2) a ->
        let t, t_ctx = type_check acc1 a in
        let e, e_ctx' = value_of acc2 a in
        print_eval a t e;
        ( Ctx.merge (fun _ _ x -> x) acc1 t_ctx,
          ECtx.merge (fun _ _ x -> x) acc2 e_ctx' ))
      ctx ast_list
    |> ignore
  with LexingError e | ParsingError e ->
    print_endline e;
    exit 1

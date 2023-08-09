open Typing.Env
open Typing.Typecheck
open Evaluating.Env
open Evaluating.Eval
open Parsing.Parse

let stdlib_ctx () =
  let ast = from_file "lib/stdlib.zml" |> parse in
  let t_ctx =
    (List.fold_left (fun acc a ->
         let _, t_ctx' = type_check acc a in
         Ctx.merge (fun _ _ x -> x) acc t_ctx'))
      Ctx.empty ast
  in
  let e_ctx =
    (List.fold_left (fun acc a ->
         let _, e_ctx' = value_of acc a in
         ECtx.merge (fun _ _ x -> x) acc e_ctx'))
      ECtx.empty ast
  in
  (t_ctx, e_ctx)

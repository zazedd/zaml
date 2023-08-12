open Typing.Env
open Typing.Typecheck
open Evaluating.Env
open Evaluating.Eval
open Parsing.Parse

let stdlib_ctx () =
  let ast = from_file "lib/stdlib.zml" |> parse in
  let t_ctx, e_ctx =
    List.fold_left
      (fun (acc1, acc2) a ->
        let _, t_ctx' = type_check acc1 a in
        let _, e_ctx' = value_of acc2 a in
        ( Ctx.merge (fun _ _ x -> x) acc1 t_ctx',
          ECtx.merge (fun _ _ x -> x) acc2 e_ctx' ))
      (Ctx.empty, ECtx.empty) ast
  in
  (t_ctx, e_ctx)

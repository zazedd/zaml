open Ast.Typed
open Parsing.Parse
open Typing.Env
open Typing.Typecheck

let run file =
  let lines = from_file file in
  let ast_list = lines |> parse in
  List.fold_left
    (fun acc a ->
      let t, t_ctx = type_check acc a in
      "- : " ^ string_of_typ t |> print_endline;
      Ctx.merge (fun _ _ x -> x) acc t_ctx)
    Ctx.empty ast_list

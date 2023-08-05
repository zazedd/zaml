open Ast.Typed
open Typing.Env

let generic_level = 100000000

let%test "unit" =
  match
    Parsing.Parse.parse "()" |> List.hd
    |> Typing.Typecheck.type_check Ctx.empty
    |> fst
  with
  | TUnit -> true
  | _ -> false

let%test "int" =
  match
    Parsing.Parse.parse "10" |> List.hd
    |> Typing.Typecheck.type_check Ctx.empty
    |> fst
  with
  | TInt -> true
  | _ -> false

let%test "bool" =
  match
    Parsing.Parse.parse "true" |> List.hd
    |> Typing.Typecheck.type_check Ctx.empty
    |> fst
  with
  | TBool -> true
  | _ -> false

let%test "let lambda fun" =
  match
    Parsing.Parse.parse "let f = fun x -> x"
    |> List.hd
    |> Typing.Typecheck.type_check Ctx.empty
    |> fst
  with
  | TArrow
      ( TVar { contents = Unbound ("'a", 100000000) },
        TVar { contents = Unbound ("'a", 100000000) },
        { new_level = 100000000; old_level = 100000000 } ) ->
      true
  | _ -> false

let%test "let fun" =
  match
    Parsing.Parse.parse "let f x = x"
    |> List.hd
    |> Typing.Typecheck.type_check Ctx.empty
    |> fst
  with
  | TArrow
      ( TVar { contents = Unbound ("'a", 1) },
        TVar { contents = Unbound ("'a", 1) },
        { new_level = 1; old_level = 1 } ) ->
      true
  | _ -> false

let%test "int variable" =
  match
    Parsing.Parse.parse "let a = 9"
    |> List.hd
    |> Typing.Typecheck.type_check Ctx.empty
    |> fst
  with
  | TInt -> true
  | _ -> false

let%test "bool variable" =
  match
    Parsing.Parse.parse "let a = false"
    |> List.hd
    |> Typing.Typecheck.type_check Ctx.empty
    |> fst
  with
  | TBool -> true
  | _ -> false

let%test "let in" =
  match
    Parsing.Parse.parse "let a = 420 in a"
    |> List.hd
    |> Typing.Typecheck.type_check Ctx.empty
    |> fst
  with
  | TInt -> true
  | _ -> false

let%test "apply" =
  match
    Parsing.Parse.parse "let a = let f b = b in f true"
    |> List.hd
    |> Typing.Typecheck.type_check Ctx.empty
    |> fst
  with
  | TVar { contents = Link TBool } -> true
  | _ -> false

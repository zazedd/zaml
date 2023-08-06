open Ast.Typed
open Typing.Env
open Typing.Errors

let run () = ()

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

let%test "unit variable" =
  match
    Parsing.Parse.parse "let a = ()"
    |> List.hd
    |> Typing.Typecheck.type_check Ctx.empty
    |> fst
  with
  | TUnit -> true
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

let%test "wrong_application" =
  match
    Parsing.Parse.parse "let _ = 1 2"
    |> List.hd
    |> Typing.Typecheck.type_check Ctx.empty
  with
  | exception TypeError _ -> true
  | (exception _) | _ -> false

let%test "wrong_application2" =
  match
    List.nth (Parsing.Parse.parse "let f a = a; let _ = 1 f") 1
    |> Typing.Typecheck.type_check Ctx.empty
  with
  | exception TypeError _ -> true
  | (exception _) | _ -> false

let%test "occur_error1" =
  match
    Parsing.Parse.parse "let _ = fun y -> y (fun z -> y z)"
    |> List.hd
    |> Typing.Typecheck.type_check Ctx.empty
  with
  | exception OccurCheck _ -> true
  | (exception _) | _ -> false

let%test "occur_error2" =
  match
    Parsing.Parse.parse "let f y = y (fun z -> y z)"
    |> List.hd
    |> Typing.Typecheck.type_check Ctx.empty
  with
  | exception OccurCheck _ -> true
  | (exception _) | _ -> false

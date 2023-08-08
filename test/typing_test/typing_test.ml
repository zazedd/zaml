open Ast.Typed
open Typing.Env
open Typing.Errors

let run () = ()

let%test "unit" =
  match
    Parsing.Parse.from_string "()"
    |> Parsing.Parse.parse |> List.hd
    |> Typing.Typecheck.type_check Ctx.empty
    |> fst
  with
  | TUnit -> true
  | _ -> false

let%test "int" =
  match
    Parsing.Parse.from_string "10"
    |> Parsing.Parse.parse |> List.hd
    |> Typing.Typecheck.type_check Ctx.empty
    |> fst
  with
  | TInt -> true
  | _ -> false

let%test "bool" =
  match
    Parsing.Parse.from_string "true"
    |> Parsing.Parse.parse |> List.hd
    |> Typing.Typecheck.type_check Ctx.empty
    |> fst
  with
  | TBool -> true
  | _ -> false

let%test "let lambda fun" =
  match
    Parsing.Parse.from_string "let f = fun x -> x"
    |> Parsing.Parse.parse |> List.hd
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
    Parsing.Parse.from_string "let f x = x"
    |> Parsing.Parse.parse |> List.hd
    |> Typing.Typecheck.type_check Ctx.empty
    |> fst |> string_of_typ
  with
  | "'a -> 'a" -> true
  | _ -> false

let%test "int variable" =
  match
    Parsing.Parse.from_string "let a = 9"
    |> Parsing.Parse.parse |> List.hd
    |> Typing.Typecheck.type_check Ctx.empty
    |> fst
  with
  | TInt -> true
  | _ -> false

let%test "bool variable" =
  match
    Parsing.Parse.from_string "let a = false"
    |> Parsing.Parse.parse |> List.hd
    |> Typing.Typecheck.type_check Ctx.empty
    |> fst
  with
  | TBool -> true
  | _ -> false

let%test "unit variable" =
  match
    Parsing.Parse.from_string "let a = ()"
    |> Parsing.Parse.parse |> List.hd
    |> Typing.Typecheck.type_check Ctx.empty
    |> fst
  with
  | TUnit -> true
  | _ -> false

let%test "let in" =
  match
    Parsing.Parse.from_string "let a = 420 in a"
    |> Parsing.Parse.parse |> List.hd
    |> Typing.Typecheck.type_check Ctx.empty
    |> fst
  with
  | TInt -> true
  | _ -> false

let%test "apply" =
  match
    Parsing.Parse.from_string "let a = let f b = b in f true"
    |> Parsing.Parse.parse |> List.hd
    |> Typing.Typecheck.type_check Ctx.empty
    |> fst
  with
  | TVar { contents = Link TBool } -> true
  | _ -> false

let%test "if1" =
  match
    Parsing.Parse.from_string "fun a b c -> if a then b else c"
    |> Parsing.Parse.parse |> List.hd
    |> Typing.Typecheck.type_check Ctx.empty
    |> fst |> string_of_typ
  with
  | "bool -> 'a -> 'a -> 'a" -> true
  | _ -> false

let%test "if2" =
  match
    Parsing.Parse.from_string "fun a b -> if a then b else 1"
    |> Parsing.Parse.parse |> List.hd
    |> Typing.Typecheck.type_check Ctx.empty
    |> fst |> string_of_typ
  with
  | "bool -> int -> int" -> true
  | _ -> false

let%test "if3" =
  match
    Parsing.Parse.from_string "fun a b -> if a then false else b"
    |> Parsing.Parse.parse |> List.hd
    |> Typing.Typecheck.type_check Ctx.empty
    |> fst |> string_of_typ
  with
  | "bool -> bool -> bool" -> true
  | _ -> false

let%test "fun1" =
  match
    Parsing.Parse.from_string "fun x -> fun y -> fun k -> k (k x y) (k y x)"
    |> Parsing.Parse.parse |> List.hd
    |> Typing.Typecheck.type_check Ctx.empty
    |> fst |> string_of_typ
  with
  | "'a -> 'a -> ('a -> 'a -> 'a) -> 'a" -> true
  | _ -> false

let%test "fun2" =
  match
    Parsing.Parse.from_string "fun x -> fun y -> let x = x y in fun x -> y x"
    |> Parsing.Parse.parse |> List.hd
    |> Typing.Typecheck.type_check Ctx.empty
    |> fst |> string_of_typ
  with
  | "(('a -> 'b) -> 'c) -> ('a -> 'b) -> 'a -> 'b" -> true
  | _ -> false

let%test "fun2" =
  match
    Parsing.Parse.from_string
      "fun x -> let y = let z = x (fun x -> x) in z in y"
    |> Parsing.Parse.parse |> List.hd
    |> Typing.Typecheck.type_check Ctx.empty
    |> fst |> string_of_typ
  with
  | "(('a -> 'a) -> 'b) -> 'b" -> true
  | _ -> false

let%test "wrong_application" =
  match
    Parsing.Parse.from_string "let _ = 1 2"
    |> Parsing.Parse.parse |> List.hd
    |> Typing.Typecheck.type_check Ctx.empty
  with
  | exception TypeError _ -> true
  | (exception _) | _ -> false

let%test "wrong_application2" =
  match
    List.nth
      (Parsing.Parse.from_string "let f a = a; let _ = 1 f"
      |> Parsing.Parse.parse)
      1
    |> Typing.Typecheck.type_check Ctx.empty
  with
  | exception TypeError _ -> true
  | (exception _) | _ -> false

let%test "occur_error1" =
  match
    Parsing.Parse.from_string "let _ = fun y -> y (fun z -> y z)"
    |> Parsing.Parse.parse |> List.hd
    |> Typing.Typecheck.type_check Ctx.empty
  with
  | exception OccurCheck _ -> true
  | (exception _) | _ -> false

let%test "occur_error2" =
  match
    Parsing.Parse.from_string "let f y = y (fun z -> y z)"
    |> Parsing.Parse.parse |> List.hd
    |> Typing.Typecheck.type_check Ctx.empty
  with
  | exception OccurCheck _ -> true
  | (exception _) | _ -> false

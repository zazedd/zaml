open Ast.Typed
open Typing.Env
open Typing.Errors

let start_time = Sys.time ()
let run () = ()

let test str ctx =
  Parsing.Parse.from_string str
  |> Parsing.Parse.parse |> List.hd
  |> Typing.Typecheck.type_check ctx
  |> fst

let%test "unit" = match test "()" Ctx.empty with TUnit -> true | _ -> false
let%test "int" = match test "10" Ctx.empty with TInt -> true | _ -> false
let%test "bool" = match test "true" Ctx.empty with TBool -> true | _ -> false
let%test "bop+" = match test "1 + 2" Ctx.empty with TInt -> true | _ -> false
let%test "bop*" = match test "1 * 2" Ctx.empty with TInt -> true | _ -> false

let%test "bop==" =
  match test "1 == 2" Ctx.empty with TBool -> true | _ -> false

let%test "bop-" = match test "1 - 2" Ctx.empty with TInt -> true | _ -> false
let%test "bop/" = match test "1 / 2" Ctx.empty with TInt -> true | _ -> false
let%test "bop%" = match test "1 % 2" Ctx.empty with TInt -> true | _ -> false

let%test "fun bop+" =
  match test "let f a = a + 1" Ctx.empty |> string_of_typ with
  | "int -> int" -> true
  | _ -> false

let%test "fun bop+2" =
  match test "let f a = 1 + a" Ctx.empty |> string_of_typ with
  | "int -> int" -> true
  | _ -> false

let%test "fun bop*" =
  match test "let f a = 1 * a" Ctx.empty |> string_of_typ with
  | "int -> int" -> true
  | _ -> false

let%test "fun bop*2" =
  match test "let f a = a * 1" Ctx.empty |> string_of_typ with
  | "int -> int" -> true
  | _ -> false

let%test "fun bop==" =
  match test "let f a = a == 1" Ctx.empty |> string_of_typ with
  | "int -> bool" -> true
  | _ -> false

let%test "fun bop==2" =
  match test "let f a = 1 == a" Ctx.empty |> string_of_typ with
  | "int -> bool" -> true
  | _ -> false

let%test "fun bop-" =
  match test "let f a = a - 1" Ctx.empty |> string_of_typ with
  | "int -> int" -> true
  | _ -> false

let%test "fun bop-2" =
  match test "let f a = 1 - a" Ctx.empty |> string_of_typ with
  | "int -> int" -> true
  | _ -> false

let%test "fun bop/" =
  match test "let f a = a / 1" Ctx.empty |> string_of_typ with
  | "int -> int" -> true
  | _ -> false

let%test "fun bop/2" =
  match test "let f a = 1 / a" Ctx.empty |> string_of_typ with
  | "int -> int" -> true
  | _ -> false

let%test "fun bop%" =
  match test "let f a = a % 1" Ctx.empty |> string_of_typ with
  | "int -> int" -> true
  | _ -> false

let%test "fun bop%2" =
  match test "let f a = 1 % a" Ctx.empty |> string_of_typ with
  | "int -> int" -> true
  | _ -> false

let%test "let lambda fun" =
  match test "let f = fun x -> x" Ctx.empty with
  | TArrow
      ( TVar { contents = Unbound ("'a", 100000000) },
        TVar { contents = Unbound ("'a", 100000000) },
        { new_level = 100000000; old_level = 100000000 } ) ->
      true
  | _ -> false

let%test "let fun" =
  match test "let f x = x" Ctx.empty |> string_of_typ with
  | "'a -> 'a" -> true
  | _ -> false

let%test "int variable" =
  match test "let a = 9" Ctx.empty with TInt -> true | _ -> false

let%test "bool variable" =
  match test "let a = false" Ctx.empty with TBool -> true | _ -> false

let%test "unit variable" =
  match test "let a = ()" Ctx.empty with TUnit -> true | _ -> false

let%test "let in" =
  match test "let a = 420 in a" Ctx.empty with TInt -> true | _ -> false

let%test "apply" =
  match test "let a = let f b = b in f true" Ctx.empty with
  | TVar { contents = Link TBool } -> true
  | _ -> false

let%test "if1" =
  match test "fun a b c -> if a then b else c" Ctx.empty |> string_of_typ with
  | "bool -> 'a -> 'a -> 'a" -> true
  | _ -> false

let%test "if2" =
  match test "fun a b -> if a then b else 1" Ctx.empty |> string_of_typ with
  | "bool -> int -> int" -> true
  | _ -> false

let%test "if3" =
  match test "fun a b -> if a then false else b" Ctx.empty |> string_of_typ with
  | "bool -> bool -> bool" -> true
  | _ -> false

let%test "fun1" =
  match
    test "fun x -> fun y -> fun k -> k (k x y) (k y x)" Ctx.empty
    |> string_of_typ
  with
  | "'a -> 'a -> ('a -> 'a -> 'a) -> 'a" -> true
  | _ -> false

let%test "fun2" =
  match
    test "fun x -> fun y -> let x = x y in fun x -> y x" Ctx.empty
    |> string_of_typ
  with
  | "(('a -> 'b) -> 'c) -> ('a -> 'b) -> 'a -> 'b" -> true
  | _ -> false

let%test "fun2" =
  match
    test "fun x -> let y = let z = x (fun x -> x) in z in y" Ctx.empty
    |> string_of_typ
  with
  | "(('a -> 'a) -> 'b) -> 'b" -> true
  | _ -> false

let%test "if_branches1" =
  match
    test "let f a b = if a == 2 then b else b + 1" Ctx.empty |> string_of_typ
  with
  | "int -> int -> int" -> true
  | _ -> false

let%test "if_branches1" =
  match
    test "let f a b = if a == 2 then b + 1 else b" Ctx.empty |> string_of_typ
  with
  | "int -> int -> int" -> true
  | _ -> false

let%test "partial app" =
  let _, ctx =
    Parsing.Parse.from_string "let f a b = if a == 2 then b + 1 else b"
    |> Parsing.Parse.parse |> List.hd
    |> Typing.Typecheck.type_check Ctx.empty
  in
  match test "f 1" ctx |> string_of_typ with "int -> int" -> true | _ -> false

let%test "wrong_application" =
  match test "let _ = 1 2" Ctx.empty with
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
  match test "let _ = fun y -> y (fun z -> y z)" Ctx.empty with
  | exception OccurCheck _ -> true
  | (exception _) | _ -> false

let%test "occur_error2" =
  match test "let f y = y (fun z -> y z)" Ctx.empty with
  | exception OccurCheck _ -> true
  | (exception _) | _ -> false

let%test "cycle_free bug" =
  match test "let f a b = if a then b else 1; f true true; f" Ctx.empty with
  | exception _ -> false
  | _ -> true

let () =
  "Typing tests completed. Time elapsed: "
  ^ string_of_float ((Sys.time () -. start_time) *. 1000.)
  ^ "ms"
  |> print_endline

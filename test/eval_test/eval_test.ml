open Evaluating.Env
open Evaluating.Eval

let start_time = Sys.time ()
let run () = ()

let test str ctx =
  Parsing.Parse.from_string str
  |> Parsing.Parse.parse |> List.hd
  |> Evaluating.Eval.value_of ctx
  |> fst

let%test "function" =
  let ctx =
    Parsing.Parse.from_string "let f1 x y k = k (k x y) (k y x)"
    |> Parsing.Parse.parse
    |> List.fold_left
         (fun acc a ->
           let _, newctx = Evaluating.Eval.value_of acc a in
           ECtx.merge (fun _ _ x -> x) acc newctx)
         ECtx.empty
  in
  match test "f1 5 10 (fun a b -> a + b)" ctx |> string_of_val with
  | "30" -> true
  | _ -> false

let%test "one lambda" =
  let ctx =
    Parsing.Parse.from_string "let f1 = fun x y k -> k (k x y) (k y x)"
    |> Parsing.Parse.parse
    |> List.fold_left
         (fun acc a ->
           let _, newctx = Evaluating.Eval.value_of acc a in
           ECtx.merge (fun _ _ x -> x) acc newctx)
         ECtx.empty
  in
  match test "f1 5 10 (fun a b -> a + b)" ctx |> string_of_val with
  | "30" -> true
  | _ -> false

let%test "chained lambdas" =
  let ctx =
    Parsing.Parse.from_string
      "let f1 = fun x -> fun y -> fun k -> k (k x y) (k y x)"
    |> Parsing.Parse.parse
    |> List.fold_left
         (fun acc a ->
           let _, newctx = Evaluating.Eval.value_of acc a in
           ECtx.merge (fun _ _ x -> x) acc newctx)
         ECtx.empty
  in
  match test "f1 5 10 (fun a b -> a + b)" ctx |> string_of_val with
  | "30" -> true
  | _ -> false

let%test "equals" =
  let ctx =
    Parsing.Parse.from_string "let f a b = a == b"
    |> Parsing.Parse.parse
    |> List.fold_left
         (fun acc a ->
           let _, newctx = Evaluating.Eval.value_of acc a in
           ECtx.merge (fun _ _ x -> x) acc newctx)
         ECtx.empty
  in
  match
    ( test "f 1 1" ctx |> string_of_val,
      test "f 1 2" ctx |> string_of_val,
      test "f \"a\" \"a\"" ctx |> string_of_val,
      test "f \"a\" \"b\"" ctx |> string_of_val,
      test "f true true" ctx |> string_of_val,
      test "f true false" ctx |> string_of_val,
      test "f 'a' 'a'" ctx |> string_of_val,
      test "f 'a' 'b'" ctx |> string_of_val )
  with
  | "true", "false", "true", "false", "true", "false", "true", "false" -> true
  | _ -> false

let () =
  "Eval tests completed. Time elapsed: "
  ^ string_of_float ((Sys.time () -. start_time) *. 1000.)
  ^ "ms"
  |> print_endline

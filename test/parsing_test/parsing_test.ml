open Ast.Parsed

let run () = ()

let%test "unit" =
  match Parsing.Parse.parse "()" |> List.hd with Unit -> true | _ -> false

let%test "int" =
  match Parsing.Parse.parse "10" |> List.hd with Int 10 -> true | _ -> false

let%test "bool" =
  match Parsing.Parse.parse "true" |> List.hd with
  | Bool true -> true
  | _ -> false

let%test "let lambda fun" =
  match Parsing.Parse.parse "let f = fun x -> x" |> List.hd with
  | Let
      {
        name = "f";
        binding = Lambda { vars = [ "x" ]; body = Var "x" };
        in_body = None;
      } ->
      true
  | _ -> false

let%test "let fun" =
  match Parsing.Parse.parse "let f x = x" |> List.hd with
  | Fun { name = "f"; vars = [ "x" ]; binding = Var "x"; in_body = None } ->
      true
  | _ -> false

let%test "int variable" =
  match Parsing.Parse.parse "let a = 9" |> List.hd with
  | Let { name = "a"; binding = Int 9; in_body = None } -> true
  | _ -> false

let%test "bool variable" =
  match Parsing.Parse.parse "let a = false" |> List.hd with
  | Let { name = "a"; binding = Bool false; in_body = None } -> true
  | _ -> false

let%test "unit variable" =
  match Parsing.Parse.parse "let a = ()" |> List.hd with
  | Let { name = "a"; binding = Unit; in_body = None } -> true
  | _ -> false

let%test "let in" =
  match Parsing.Parse.parse "let a = 420 in a" |> List.hd with
  | Let { name = "a"; binding = Int 420; in_body = Some (Var "a") } -> true
  | _ -> false

let%test "apply" =
  match Parsing.Parse.parse "let a = let f b = b in f true" |> List.hd with
  | Let
      {
        name = "a";
        binding =
          Fun
            {
              name = "f";
              vars = [ "b" ];
              binding = Var "b";
              in_body = Some (App (Var "f", Bool true));
            };
        in_body = None;
      } ->
      true
  | _ -> false

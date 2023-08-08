open Ast.Parsed

let start_time = Sys.time ()
let run () = ()

let%test "unit" =
  match
    Parsing.Parse.from_string "()" |> Parsing.Parse.parse |> List.hd |> get_expr
  with
  | Unit -> true
  | _ -> false

let%test "int" =
  match
    Parsing.Parse.from_string "10" |> Parsing.Parse.parse |> List.hd |> get_expr
  with
  | Int 10 -> true
  | _ -> false

let%test "bool" =
  match
    Parsing.Parse.from_string "true"
    |> Parsing.Parse.parse |> List.hd |> get_expr
  with
  | Bool true -> true
  | _ -> false

let%test "bop+" =
  match
    Parsing.Parse.from_string "1 + 2"
    |> Parsing.Parse.parse |> List.hd |> get_expr
  with
  | Bop (Add, { expr = Int 1; _ }, { expr = Int 2; _ }) -> true
  | _ -> false

let%test "bop*" =
  match
    Parsing.Parse.from_string "1 * 2"
    |> Parsing.Parse.parse |> List.hd |> get_expr
  with
  | Bop (Mult, { expr = Int 1; _ }, { expr = Int 2; _ }) -> true
  | _ -> false

let%test "bop==" =
  match
    Parsing.Parse.from_string "1 == 2"
    |> Parsing.Parse.parse |> List.hd |> get_expr
  with
  | Bop (Eq, { expr = Int 1; _ }, { expr = Int 2; _ }) -> true
  | _ -> false

let%test "let lambda fun" =
  match
    Parsing.Parse.from_string "let f = fun x -> x"
    |> Parsing.Parse.parse |> List.hd |> get_expr
  with
  | Let
      {
        name = "f";
        binding =
          { expr = Lambda { vars = [ "x" ]; body = { expr = Var "x"; _ } }; _ };
        in_body = None;
      } ->
      true
  | _ -> false

let%test "let fun" =
  match
    Parsing.Parse.from_string "let f x = x"
    |> Parsing.Parse.parse |> List.hd |> get_expr
  with
  | Let
      {
        name = "f";
        binding =
          {
            expr = Lambda { vars = [ "x" ]; body = { expr = Var "x"; _ }; _ };
            _;
          };
        in_body = None;
      } ->
      true
  | _ -> false

let%test "int variable" =
  match
    Parsing.Parse.from_string "let a = 9"
    |> Parsing.Parse.parse |> List.hd |> get_expr
  with
  | Let { name = "a"; binding = { expr = Int 9; _ }; in_body = None } -> true
  | _ -> false

let%test "bool variable" =
  match
    Parsing.Parse.from_string "let a = false"
    |> Parsing.Parse.parse |> List.hd |> get_expr
  with
  | Let { name = "a"; binding = { expr = Bool false; _ }; in_body = None } ->
      true
  | _ -> false

let%test "unit variable" =
  match
    Parsing.Parse.from_string "let a = ()"
    |> Parsing.Parse.parse |> List.hd |> get_expr
  with
  | Let { name = "a"; binding = { expr = Unit; _ }; in_body = None } -> true
  | _ -> false

let%test "let in" =
  match
    Parsing.Parse.from_string "let a = 420 in a"
    |> Parsing.Parse.parse |> List.hd |> get_expr
  with
  | Let
      {
        name = "a";
        binding = { expr = Int 420; _ };
        in_body = Some { expr = Var "a"; _ };
      } ->
      true
  | _ -> false

let () =
  "Parsing tests completed sucessfuly. Time elapsed: "
  ^ string_of_float ((Sys.time () -. start_time) *. 1000.)
  ^ "ms"
  |> print_endline

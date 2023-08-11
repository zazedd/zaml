open Ast.Parsed

let start_time = Sys.time ()
let run () = ()

let test str =
  Parsing.Parse.from_string str |> Parsing.Parse.parse |> List.hd |> get_expr

let%test "unit" = match test "()" with Const Unit -> true | _ -> false
let%test "int" = match test "10" with Const (Int 10) -> true | _ -> false
let%test "char" = match test "'a'" with Const (Char 'a') -> true | _ -> false

let%test "string" =
  match test "\"testing\"" with Const (String "testing") -> true | _ -> false

let%test "bool" =
  match test "true" with Const (Bool true) -> true | _ -> false

let%test "int list" =
  match test "[1; 2]" with
  | Const (List [ { expr = Const (Int 1); _ }; { expr = Const (Int 2); _ } ]) ->
      true
  | _ -> false

let%test "string list" =
  match test "[\"str\"; \"str2\"]" with
  | Const
      (List
        [
          { expr = Const (String "str"); _ };
          { expr = Const (String "str2"); _ };
        ]) ->
      true
  | _ -> false

let%test "bop+" =
  match test "1 + 2" with
  | Bop (Add, { expr = Const (Int 1); _ }, { expr = Const (Int 2); _ }) -> true
  | _ -> false

let%test "bop*" =
  match test "1 * 2" with
  | Bop (Mult, { expr = Const (Int 1); _ }, { expr = Const (Int 2); _ }) -> true
  | _ -> false

let%test "bop==" =
  match test "1 == 2" with
  | Bop (Eq, { expr = Const (Int 1); _ }, { expr = Const (Int 2); _ }) -> true
  | _ -> false

let%test "let lambda fun" =
  match test "let f = fun x -> x" with
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
  match test "let f x = x" with
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
  match test "let a = 9" with
  | Let { name = "a"; binding = { expr = Const (Int 9); _ }; in_body = None } ->
      true
  | _ -> false

let%test "char variable" =
  match test "let a = 'a'" with
  | Let { name = "a"; binding = { expr = Const (Char 'a'); _ }; in_body = None }
    ->
      true
  | _ -> false

let%test "string variable" =
  match test "let a = \"testing stuff\"" with
  | Let
      {
        name = "a";
        binding = { expr = Const (String "testing stuff"); _ };
        in_body = None;
      } ->
      true
  | _ -> false

let%test "bool variable" =
  match test "let a = false" with
  | Let
      { name = "a"; binding = { expr = Const (Bool false); _ }; in_body = None }
    ->
      true
  | _ -> false

let%test "unit variable" =
  match test "let a = ()" with
  | Let { name = "a"; binding = { expr = Const Unit; _ }; in_body = None } ->
      true
  | _ -> false

let%test "let in" =
  match test "let a = 420 in a" with
  | Let
      {
        name = "a";
        binding = { expr = Const (Int 420); _ };
        in_body = Some { expr = Var "a"; _ };
      } ->
      true
  | _ -> false

let () =
  "Parsing tests completed. Time elapsed: "
  ^ string_of_float ((Sys.time () -. start_time) *. 1000.)
  ^ "ms"
  |> print_endline

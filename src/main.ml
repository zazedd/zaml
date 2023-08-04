open Typing.Env

let handle_args () =
  match Array.length Sys.argv with
  | 1 -> Repl.Cli.repl Ctx.empty
  | 2 ->
      let _ = Sys.argv.(1) in
      assert false
  | _ -> failwith "Too many arguments"

let () = handle_args ()

(* let rec string_of_ast = function *)
(*   | Unit -> "unit" *)
(*   | Int i -> "int = " ^ string_of_int i *)
(*   | Bool b -> "bool = " ^ string_of_bool b *)
(*   | Var s -> "var name = " ^ s *)
(*   | Let { name; binding; in_body } -> ( *)
(*       "let = name = " ^ name ^ "\n" ^ string_of_ast binding *)
(*       ^ match in_body with None -> "" | Some b -> string_of_ast b) *)
(*   | Lambdabda { vars; body } -> *)
(*       List.fold_left (fun acc a -> acc ^ " " ^ a) "Lambdabda =\nvars = \n" vars *)
(*       ^ "\nbody = " ^ string_of_ast body *)
(*   | Fun { name; vars; binding; in_body } -> ( *)
(*       "Fun name = " ^ name *)
(*       ^ List.fold_left (fun acc a -> acc ^ " " ^ a) "\nvars = \n" vars *)
(*       ^ "\nbinding = " ^ string_of_ast binding ^ "\nbody = " *)
(*       ^ match in_body with None -> "" | Some b -> string_of_ast b) *)
(*   | App (e1, e2) -> "App = " ^ string_of_ast e1 ^ " and " ^ string_of_ast e2 *)

(* let id = Lambda { vars = [ "x" ]; body = Var "x" } *)
(***)
(* let () = *)
(*   assert ( *)
(*     TArrow *)
(*       ( TVar { contents = Unbound ("'a", 1) }, *)
(*         TVar { contents = Unbound ("'a", 1) }, *)
(*         { old_level = 1; new_level = 1 } ) *)
(*     = top_type_check id) *)
(***)

open Ast.Parsed
open Env
open Errors

let is_value = function
  | VUnit | VInt _ | VChar _ | VString _ | VBool _ -> true
  | _ -> false

let string_of_val = function
  | VUnit -> "()"
  | VInt i -> string_of_int i
  | VChar c -> "'" ^ String.make 1 c ^ "'"
  | VString s -> "\"" ^ s ^ "\""
  | VBool b -> string_of_bool b
  | Closure _ -> not_a_value ()

let lookup ctx v =
  try (ECtx.find v ctx, ctx) with Not_found -> unbound_variable v

let rec value_of ctx e =
  match get_expr e with
  | Const c -> eval_const ctx c
  | Var v -> lookup ctx v
  | If (e1, e2, e3) -> eval_if ctx e1 e2 e3
  | Bop (op, e1, e2) -> eval_bop ctx op e1 e2
  | Let { name; binding; in_body } -> eval_let ctx name binding in_body
  | Lambda { vars; body } -> (follow_lambda vars body ctx, ctx)
  | App (e1, e2) -> eval_app ctx e1 e2

and eval_const ctx = function
  | Unit -> (VUnit, ctx)
  | Int v -> (VInt v, ctx)
  | Char c -> (VChar c, ctx)
  | String s -> (VString s, ctx)
  | Bool b -> (VBool b, ctx)

and eval_let ctx name binding in_body =
  let v1, newctx = value_of ctx binding in
  let ctx' = ECtx.add name v1 newctx in
  match in_body with
  | None -> (v1, ctx')
  | Some e ->
      let v2, _ = value_of ctx' e in
      (v2, ctx')

and eval_if ctx e1 e2 e3 =
  let e, ctx' = value_of ctx e1 in
  match e with
  | VBool true -> value_of ctx' e2
  | VBool false -> value_of ctx' e3
  | _ -> if_guard_error ()

and eval_bop ctx op e1 e2 =
  match (op, value_of ctx e1 |> fst, value_of ctx e2 |> fst) with
  | Add, VInt a, VInt b -> (VInt (a + b), ctx)
  | Subt, VInt a, VInt b -> (VInt (a - b), ctx)
  | Mult, VInt a, VInt b -> (VInt (a * b), ctx)
  | Div, VInt a, VInt b -> (VInt (a / b), ctx)
  | Mod, VInt a, VInt b -> (VInt (a mod b), ctx)
  | Eq, VInt a, VInt b -> (VBool (a = b), ctx)
  | _ -> op_error ()

and follow_lambda vars body ctx =
  match get_expr body with
  | Lambda { vars = vars_inside; body = body_inside } ->
      let newvars = List.append vars vars_inside in
      follow_lambda newvars body_inside ctx
  | _ -> Closure { vars; body; context = ctx }

and eval_app ctx e1 e2 =
  let e, ctx' = value_of ctx e1 in
  match e with
  | Closure { vars; body; context } -> (
      match compare (List.length vars) (List.length e2) with
      | 0 ->
          let body_env =
            List.fold_left2
              (fun acc var exp ->
                let v, _ = value_of ctx' exp in
                ECtx.add var v acc)
              context vars e2
          in
          (value_of body_env body |> fst, ctx)
      | a when a > 0 ->
          (* partial application *)
          let binded_vars = get_binded_vars (List.length e2) [] vars in
          let remaining_vars = get_remaining_vars (List.length e2) vars in
          let body_env =
            List.fold_left2
              (fun acc var exp ->
                let v, _ = value_of ctx' exp in
                ECtx.add var v acc)
              context binded_vars e2
          in
          let partial_closure =
            Closure { vars = remaining_vars; body; context = body_env }
          in
          (partial_closure, ctx)
      | _ -> too_many_args (List.length vars))
  | _ -> app_error ()

and get_binded_vars n acc lst =
  if n <= 0 then acc |> List.rev
  else
    match lst with
    | [] -> acc
    | x :: xs -> get_binded_vars (n - 1) (x :: acc) xs

and get_remaining_vars n lst =
  if n <= 0 then lst
  else match lst with [] -> [] | _ :: xs -> get_remaining_vars (n - 1) xs

open Ast.Parsed
open Env
open Errors

let is_value = function VUnit | VInt _ | VBool _ -> true | _ -> false

let string_of_val = function
  | VUnit -> "()"
  | VInt i -> string_of_int i
  | VBool b -> string_of_bool b
  | Closure _ -> failwith "Not a value"

let lookup ctx v =
  try (ECtx.find v ctx, ctx) with Not_found -> unbound_variable v

let rec value_of ctx e =
  match get_expr e with
  | Unit -> (VUnit, ctx)
  | Int v -> (VInt v, ctx)
  | Bool v -> (VBool v, ctx)
  | Var v -> lookup ctx v
  | If (e1, e2, e3) -> eval_if ctx e1 e2 e3
  | Bop (op, e1, e2) -> eval_bop ctx op e1 e2
  | Let { name; binding; in_body } -> eval_let ctx name binding in_body
  | Lambda { vars; body } -> (Closure { vars; body; context = ctx }, ctx)
  | App (e1, e2) -> eval_app ctx e1 e2

(* and eval_bop ctx op e1 e2 = *)
(*   match (op, eval ctx e1 |> fst, eval ctx e2 |> fst) with *)
(*   | Add, VInt a, VInt b -> (VInt (a + b), ctx) *)
(*   | Mult, VInt a, VInt b -> (VInt (a * b), ctx) *)
(*   | Eq, VInt a, VInt b -> (VBool (a = b), ctx) *)
(*   | _ -> RuntimeError "Operator requires the same type on both sides" |> raise *)

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
  | Mult, VInt a, VInt b -> (VInt (a * b), ctx)
  | Eq, VInt a, VInt b -> (VBool (a = b), ctx)
  | _ -> op_error ()

and eval_app ctx e1 e2 =
  let e, ctx' = value_of ctx e1 in
  match e with
  | Closure { vars; body; context } ->
      if List.length vars <> List.length e2 then partial_app_error ();
      let body_env =
        List.fold_left2
          (fun acc var exp ->
            let v, _ = value_of ctx' exp in
            ECtx.add var v acc)
          context vars e2
      in
      (value_of body_env body |> fst, ctx)
  | _ -> app_error ()

open Env
open Unify
open Common
open Errors
open Ast.Parsed
open Ast.Typed

let lookup ctx x pos =
  try (Ctx.find x ctx |> inst, ctx) with Not_found -> unbound_variable x pos

let add_vars_ctx ctx vars =
  List.fold_left
    (fun acc a ->
      let t_arg = newvar () in
      Ctx.add a t_arg acc)
    ctx vars

let new_arrows ctx' vars t_e =
  List.fold_right
    (fun a acc ->
      let t_x = Ctx.find a ctx' in
      new_arrow t_x acc)
    vars t_e

(* Delayed occurs check to prevent cyclic types. Runs at the end of the typecheck *)
let rec cycle_free = function
  | TUnit | TInt | TChar | TBool | TVar { contents = Unbound _ } -> ()
  | TVar { contents = Link ty } -> cycle_free ty
  | TArrow (_, _, ls) when ls.new_level = marked_level ->
      occur_error "cycle_free: Variable occurs inside its definition"
  | TArrow (t1, t2, ls) ->
      let level = ls.new_level in
      ls.new_level <- marked_level;
      cycle_free t1;
      cycle_free t2;
      ls.new_level <- level

let rec typeof ctx = function
  | { expr = Const c; _ } -> typeof_const ctx c
  | { expr = Var x; pos } -> lookup ctx x pos
  | { expr = If (e1, e2, e3); pos } -> typeof_if ctx e1 e2 e3 pos
  | { expr = Bop (op, e1, e2); pos } -> typeof_bop ctx op e1 e2 pos
  | { expr = Let { name; binding; in_body }; _ } ->
      typeof_let ctx name binding in_body
  | { expr = Lambda { vars; body }; _ } -> typeof_lambda ctx vars body
  | { expr = App (e1, e2); pos } -> typeof_app ctx e1 e2 pos

and typeof_const ctx = function
  | Unit -> (TUnit, ctx)
  | Int _ -> (TInt, ctx)
  | Char _ -> (TChar, ctx)
  | Bool _ -> (TBool, ctx)

and typeof_if ctx e1 e2 e3 pos =
  match typeof ctx e1 |> fst |> head with
  | TBool -> if_branch ctx e2 e3 pos
  | TVar _ as v ->
      unify v v TBool pos;
      if_branch ctx e2 e3 pos
  | _ -> TypeError "If guard must be boolean" |> raise

and if_branch ctx e2 e3 pos =
  match (typeof ctx e2 |> fst |> head, typeof ctx e3 |> fst |> head) with
  | (TVar _ as v), t ->
      unify v v t pos;
      (t, ctx)
  | t, (TVar _ as v) ->
      unify v v t pos;
      (t, ctx)
  | t1, t2 when t1 = t2 -> (t1, ctx)
  | t1, t2 -> type_error t1 t2 pos

and typeof_bop ctx op e1 e2 pos =
  match (op, typeof ctx e1 |> fst |> head, typeof ctx e2 |> fst |> head) with
  | (Add as op), t1, t2
  | (Subt as op), t1, t2
  | (Mult as op), t1, t2
  | (Div as op), t1, t2
  | (Mod as op), t1, t2 ->
      typeof_bop_int ctx op t1 t2 pos
  | Eq, t1, t2 -> unify_bop ctx t1 t2 TInt TBool pos

and typeof_bop_int ctx op t1 t2 pos =
  match op with
  | Add | Subt | Mult | Div | Mod -> unify_bop ctx t1 t2 TInt TInt pos
  | Eq -> assert false

and typeof_let ctx name binding in_body =
  enter_level ();
  let t_e, _ = typeof ctx binding in
  leave_level ();
  gen t_e;
  let ctx' = Ctx.add name t_e ctx in
  match in_body with
  | None -> (t_e, ctx')
  | Some body -> (typeof ctx' body |> fst, ctx)

and typeof_lambda ctx vars body =
  let ctx' = add_vars_ctx ctx vars in
  enter_level ();
  let t_e, _ = typeof ctx' body in
  leave_level ();
  let t = new_arrows ctx' vars t_e in
  (t, ctx)

and typeof_app ctx e1 e2 pos =
  let t_fun = typeof ctx e1 |> fst in
  match t_fun |> head with
  | TArrow _ | TVar { contents = Unbound _ } ->
      let t_res = newvar () in
      let arrow_args =
        List.fold_right
          (fun a acc ->
            let t_arg, _ = typeof ctx a in
            new_arrow t_arg acc)
          e2 t_res
      in
      unify t_fun t_fun arrow_args pos;
      (t_res, ctx)
  | t -> unify_error t pos

let type_check ctx exp =
  reset_type_variables ();
  reset_level_adjustment ();
  let t, t_ctx = typeof ctx exp in
  cycle_free t;
  (t, t_ctx)

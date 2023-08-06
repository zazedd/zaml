open Env
open Unify
open Common
open Errors
open Ast.Parsed
open Ast.Typed

let lookup ctx x =
  try (Ctx.find x ctx |> inst, ctx) with Not_found -> unbound_variable x

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
  | TInt | TUnit | TBool | TVar { contents = Unbound _ } -> ()
  | TVar { contents = Link ty } -> cycle_free ty
  | TArrow (_, _, ls) when ls.new_level = marked_level ->
      occur_error "Variable occurs inside its definition"
  | TArrow (t1, t2, ls) ->
      let level = ls.new_level in
      ls.new_level <- marked_level;
      cycle_free t1;
      cycle_free t2;
      ls.new_level <- level

let rec typeof ctx = function
  | Unit -> (TUnit, ctx)
  | Int _ -> (TInt, ctx)
  | Bool _ -> (TBool, ctx)
  | Var x -> lookup ctx x
  | If (e1, e2, e3) -> typeof_if ctx e1 e2 e3
  | Let { name; binding; in_body } -> typeof_let ctx name binding in_body
  | Fun { name; vars; binding; in_body } ->
      typeof_fun ctx name vars binding in_body
  | Lambda { vars; body } -> typeof_lambda ctx vars body
  | App (e1, e2) -> typeof_app ctx e1 e2

and typeof_if ctx e1 e2 e3 =
  match typeof ctx e1 |> fst |> head with
  | TBool -> if_branch ctx e2 e3
  | TVar _ as v ->
      unify v TBool;
      if_branch ctx e2 e3
  | _ -> TypeError "If guard must be boolean" |> raise

and if_branch ctx e2 e3 =
  match (typeof ctx e2 |> fst |> head, typeof ctx e3 |> fst |> head) with
  | (TVar _ as v), t ->
      unify v t;
      (t, ctx)
  | t, (TVar _ as v) ->
      unify v t;
      (t, ctx)
  | t1, t2 when t1 = t2 -> (t1, ctx)
  | t1, t2 -> type_error t1 t2

and typeof_let ctx name binding in_body =
  enter_level ();
  let t_e, _ = typeof ctx binding in
  leave_level ();
  cycle_free t_e;
  gen t_e;
  let ctx' = Ctx.add name t_e ctx in
  match in_body with
  | None -> (t_e, ctx')
  | Some body -> (typeof ctx' body |> fst, ctx)

and typeof_fun ctx name vars binding in_body =
  let ctx' = add_vars_ctx ctx vars in
  enter_level ();
  let t_e, _ = typeof ctx' binding in
  leave_level ();
  let t = new_arrows ctx' vars t_e in
  let ctx'' = Ctx.add name t ctx' in
  match in_body with
  | None -> (t, ctx'')
  | Some body -> (typeof ctx'' body |> fst, ctx)

and typeof_lambda ctx vars body =
  let ctx' = add_vars_ctx ctx vars in
  let t_e, _ = typeof ctx' body in
  let t = new_arrows ctx' vars t_e in
  (t, ctx)

and typeof_app ctx e1 e2 =
  let t_fun, _ = typeof ctx e1 in
  match t_fun with
  | TArrow _
  | TVar { contents = Unbound _ }
  | TVar { contents = Link (TArrow _) } ->
      let t_arg, _ = typeof ctx e2 in
      let t_res = newvar () in
      new_arrow t_arg t_res |> unify t_fun;
      (t_res, ctx)
  | t -> unify_error t

let type_check ctx exp =
  reset_type_variables ();
  reset_level_adjustment ();
  let t, t_ctx = typeof ctx exp in
  cycle_free t;
  (t, t_ctx)

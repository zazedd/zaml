open Env
open Unify
open Common
open Errors
open Ast.Parsed
open Ast.Typed

let lookup ctx x =
  try (Ctx.find x ctx |> inst, ctx) with Not_found -> unbound_variable x

let rec typeof ctx = function
  | Unit -> (TUnit, ctx)
  | Int _ -> (TInt, ctx)
  | Bool _ -> (TBool, ctx)
  | Var x -> lookup ctx x
  | If (e1, e2, e3) -> (
      match typeof ctx e1 |> fst |> head with
      | TBool -> if_branch ctx e2 e3
      | TVar _ as v ->
          unify v TBool;
          if_branch ctx e2 e3
      | _ -> TypeError "If guard must be boolean" |> raise)
  | Let { name; binding; in_body } -> (
      enter_level ();
      let t_e, _ = typeof ctx binding in
      leave_level ();
      gen t_e;
      let ctx' = Ctx.add name t_e ctx in
      match in_body with
      | None -> (t_e, ctx')
      | Some body -> (typeof ctx' body |> fst, ctx))
  | Fun { name; vars; binding; in_body } -> (
      let t_x = newvar () in
      let ctx' = Ctx.add (vars |> List.hd) t_x ctx in
      enter_level ();
      let t_e, _ = typeof ctx' binding in
      leave_level ();
      let t = new_arrow t_x t_e in
      let ctx'' = Ctx.add name t ctx' in
      match in_body with
      | None -> (t, ctx'')
      | Some body -> (typeof ctx'' body |> fst, ctx))
  | Lambda { vars; body } ->
      let t_x = newvar () in
      let ctx' = Ctx.add (vars |> List.hd) t_x ctx in
      let t_e, _ = typeof ctx' body in
      (new_arrow t_x t_e, ctx)
  | App (e1, e2) -> (
      let t_fun, _ = typeof ctx e1 in
      match t_fun with
      | TArrow _ ->
          let t_arg, _ = typeof ctx e2 in
          let t_res = newvar () in
          new_arrow t_arg t_res |> unify t_fun;
          (t_res, ctx)
      | t -> unify_error t)

and if_branch ctx e2 e3 =
  match (typeof ctx e2 |> fst, typeof ctx e3 |> fst) with
  | TVar _, _ ->
      assert false
      (* let _ = v.def <- Some t in *)
      (* (t, ctx) *)
  | _, TVar _ ->
      assert false
      (* let _ = v.def <- Some t in *)
      (* (t, ctx) *)
  | t1, t2 when t1 = t2 -> (t1, ctx)
  | t1, t2 -> type_error t1 t2

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

let type_check ctx exp =
  reset_type_variables ();
  reset_level_adjustment ();
  let t, t_ctx = typeof ctx exp in
  cycle_free t;
  (t, t_ctx)

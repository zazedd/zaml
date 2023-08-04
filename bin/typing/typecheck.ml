open Env
open Unify
open Common
open Errors
open Ast.Parsed
open Ast.Typed

let rec typeof ctx = function
  | Unit -> TUnit
  | Int _ -> TInt
  | Bool _ -> TBool
  | Var x -> (
      try Ctx.find x ctx |> inst with Not_found -> unbound_variable x)
  | Let { name; binding; in_body } -> (
      enter_level ();
      let t_e = typeof ctx binding in
      leave_level ();
      gen t_e;
      match in_body with
      | None -> assert false
      | Some body ->
          let ctx' = Ctx.add name t_e ctx in
          typeof ctx' body)
  | Fun { name; vars; binding; in_body } -> (
      let t_x = newvar () in
      let ctx' = Ctx.add (vars |> List.hd) t_x ctx in
      enter_level ();
      let t_e = typeof ctx' binding in
      leave_level ();
      match in_body with
      | None ->
          let t = new_arrow t_x t_e in
          (* Ctx.add s t ctx', *) t
      | Some body ->
          let t = new_arrow t_x t_e in
          let ctx'' = Ctx.add name t ctx' in
          typeof ctx'' body)
  | Lambda { vars; body } ->
      let t_x = newvar () in
      let ctx' = Ctx.add (vars |> List.hd) t_x ctx in
      let t_e = typeof ctx' body in
      new_arrow t_x t_e
  | App (e1, e2) ->
      let t_fun = typeof ctx e1 in
      let t_arg = typeof ctx e2 in
      let t_res = newvar () in
      new_arrow t_arg t_res |> unify t_fun;
      t_res

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

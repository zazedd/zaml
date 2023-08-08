open Common
open Errors
open Ast.Typed

(* Main unification *)
(* Quantified variables are unexpected: they should've been instantiated *)

(*
  Ensure type level doesn't exceed l. Levels can only decrease; avoid cyclic types.
  For composite types with the same level, enqueue for later level adjustment.
  Works like a generational garbage collector.
*)
let update_level l = function
  | TUnit | TInt | TBool -> ()
  | TVar ({ contents = Unbound (n, l') } as var) ->
      assert (l' != generic_level);
      if l < l' then var := Unbound (n, l)
  | TArrow (_, _, ls) as t ->
      assert (ls.new_level != generic_level);
      if ls.new_level = marked_level then occur_error "Loop encountered";
      if l < ls.new_level then (
        if ls.new_level = ls.old_level then
          to_be_level_adjusted := t :: !to_be_level_adjusted;
        ls.new_level <- l)
  | _ -> assert false

(*
  Unifying a free variable tv with a type t takes constant time:
  it merely links var to t (setting the level of t to var if var's
  level was smaller).
  Therefore, cycles may be created accidentally,
  and the complete update of type levels may have to be done
  at a later time.
  Incidentally, another unification may need to traverse the type
  with the pending level update. That unification will do the level
  update along the way.
*)

let rec unify init t1 t2 pos =
  if t1 == t2 then () (* t1 and t2 are physically the same *)
  else
    match (head t1, head t2) with
    | ( (TVar ({ contents = Unbound (_, l1) } as var1) as t1),
        (TVar ({ contents = Unbound (_, l2) } as var2) as t2) ) ->
        if var1 == var2 then ()
        else if l1 > l2 then var1 := Link t2
        else var2 := Link t1
    | TVar ({ contents = Unbound (_, l) } as var), t'
    | t', TVar ({ contents = Unbound (_, l) } as var) ->
        update_level l t';
        var := Link t'
    | TArrow (t11, t12, ll), TArrow (t21, t22, lr) ->
        if ll.new_level = marked_level || lr.new_level = marked_level then
          occur_error "Loop encountered";
        let min_level = min ll.new_level lr.new_level in
        ll.new_level <- marked_level;
        lr.new_level <- marked_level;
        unify_lev min_level init t11 t21 pos;
        unify_lev min_level init t12 t22 pos;
        ll.new_level <- min_level;
        lr.new_level <- min_level
    | t1, t2 ->
        reset_level init;
        type_error t1 t2 pos

and unify_lev l init t1 t2 pos =
  let t1 = head t1 in
  update_level l t1;
  unify init t1 t2 pos

and reset_level t =
  match t |> head with
  | TArrow (t1, t2, l) when l.new_level = marked_level ->
      l.new_level <- l.old_level;
      reset_level t1;
      reset_level t2
  | TArrow (t1, t2, _) ->
      reset_level t1;
      reset_level t2
  | _ -> ()

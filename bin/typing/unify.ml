open Common
open Ast.Typed

(* Main unification *)
(* Quantified variables are unexpected: they should've been instantiated *)

(*
  Ensure type level doesn't exceed l. Levels can only decrease; avoid cyclic types.
  For composite types with the same level, enqueue for later level adjustment.
  Works like a generational garbage collector.
*)
let update_level l = function
  | TVar ({ contents = Unbound (n, l') } as tvr) ->
      assert (l' != generic_level);
      if l < l' then tvr := Unbound (n, l)
  | TArrow (_, _, ls) as ty ->
      assert (ls.new_level != generic_level);
      if ls.new_level = marked_level then
        failwith "occurs check, loop encountered";
      if l < ls.new_level then (
        if ls.new_level = ls.old_level then
          to_be_level_adjusted := ty :: !to_be_level_adjusted;
        ls.new_level <- l)
  | _ -> assert false

(* Unifying a free variable tv with a type t takes constant time:
   it merely links tv to t (setting the level of t to tv if tv's
   level was smaller). Therefore, cycles may be created accidentally,
   and the complete update of type levels may have to be done
   at a later time.
   Incidentally, another unification may need to traverse the type
   with the pending level update. That unification will do the level
   update along the way.
*)

let rec unify t1 t2 =
  if t1 == t2 then () (* t1 and t2 are physically the same *)
  else
    match (head t1, head t2) with
    (* unify two free vars *)
    | ( (TVar ({ contents = Unbound (_, l1) } as tv1) as t1),
        (TVar ({ contents = Unbound (_, l2) } as tv2) as t2) ) ->
        if tv1 == tv2 then () (* the same variable *)
        else if (* bind the higher-level var *)
                l1 > l2 then tv1 := Link t2
        else tv2 := Link t1
    | TVar ({ contents = Unbound (_, l) } as tv), t'
    | t', TVar ({ contents = Unbound (_, l) } as tv) ->
        update_level l t';
        tv := Link t'
    | TArrow (tyl1, tyl2, ll), TArrow (tyr1, tyr2, lr) ->
        if ll.new_level = marked_level || lr.new_level = marked_level then
          failwith "cycle: occurs check";
        let min_level = min ll.new_level lr.new_level in
        ll.new_level <- marked_level;
        lr.new_level <- marked_level;
        unify_lev min_level tyl1 tyr1;
        unify_lev min_level tyl2 tyr2;
        ll.new_level <- min_level;
        lr.new_level <- min_level
    | _ -> failwith "unification error"

and unify_lev l ty1 ty2 =
  let ty1 = head ty1 in
  update_level l ty1;
  unify ty1 ty2

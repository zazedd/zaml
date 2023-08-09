open Lib.Stdlib

let handle_args () =
  match Array.length Sys.argv with
  | 1 ->
      let t_ctx, e_ctx = stdlib_ctx () in
      Repl.Cli.run t_ctx e_ctx
  | 2 -> (
      let arg = Sys.argv.(1) in
      match Filename.extension arg with
      | ".zml" -> Interpreter.Interp.run arg
      | _ -> failwith "Please only provide .zml files")
  | _ -> failwith "Too many arguments"

let _ = handle_args ()

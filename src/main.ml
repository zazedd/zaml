open Lib.Stdlib

let handle_args () =
  match Array.length Sys.argv with
  | 1 ->
      let ctx = stdlib_ctx () in
      Repl.Cli.run ctx
  | 2 -> (
      let arg = Sys.argv.(1) in
      match Filename.extension arg with
      | ".zml" ->
          let ctx = stdlib_ctx () in
          Interpreter.Interp.run arg ctx
      | _ -> failwith "Please only provide .zml files")
  | _ -> failwith "Too many arguments"

let _ = handle_args ()

# zaml

A type inferred, statically-typed functional programming language inspired by OCaml

## Documentation

- TODO

## Running

There are two options for running zaml:

-> Running the REPL:

```bash
dune exec zaml
```

-> Interpret from a `.zml` file:

```bash
dune exec zaml example.zml
```

## Testing

```bash
dune runtest
```

## TODO list

- [x] Typechecking (Generalized Hindley-Milner (inspired by Didier Remy))
- [ ] Evaluation
- [ ] Better Syntax Errors
  - [ ] Positions
- [ ] Language features
  - [x] Let variable expressions
  - [x] Let function expressions
    - [x] Implemented
    - [x] Multiple argumrnts
  - [x] Lambda expressions
    - [x] Implemented
    - [x] Multiple arguments
  - [x] If expressions
  - [ ] Match expressions
  - [ ] Algebraic effects
  - [ ] Ranges (1..31 -> [1; 2; ... 30; 31])
  - [ ] Standard library
    - [ ] Universal `print` function
    - [ ] Figure out what else to include
  - [ ] Binary operators
    - [ ] Int ops
      - [ ] -
      - [ ] -
      - [ ] -
      - [ ] /
      - [ ] %
      - [ ] ==
      - [ ] !=
      - [ ] > , =>
      - [ ] < =<
    - [ ] String op
      - [ ] ^ (string concat)
    - [ ] Bool ops
      - [ ] !
- [ ] Types
  - [x] Unit
  - [x] Int
  - [x] Bool
  - [x] TVar
  - [x] TArrow (x -> y)
  - [ ] Strings
  - [ ] 'a list

# Assignment 8, CS3250

TODO: Update this README and fill out the reflection questions. COMPLETED

This repository contains a (template) implementation of CatML, an ML variant used at UVM for CS3250.

## Building this repository with `dune`

This repository can be built with [`dune`](https://github.com/ocaml/dune), a build system for OCaml projects. It helps manage linking when your project is made up of multiple `.ml` files.

### Install `dune`

Check if dune is already installed via the following terminal command:

```
$ dune --version
```

If the command is not found, you can install it via `opam` [(detailed instructions)](https://dune.readthedocs.io/en/latest/howto/install-dune.html).

```
$ opam install dune
$ eval $(opam config env)
$ dune --version
```

### Build the project

With `dune` installed, you can build the project by running `dune build` from the command line.

```
$ dune build
```

You can also access the project modules by running `dune utop`, which will build the project and then open the `utop` toplevel. Different files (such as `src/lang.ml`) correspond to OCaml modules, and data structures (such as the `Var` constructor in the CatML AST) can be accessed via `CatML.Lang.Bool`, where `CatML` is the overall name of the project.

### Structure of the repository

```
- <root>
  - dune-project
  - src
    - dune
    - lang.ml
    - lexer.mll
    - parser.mly
    - main.ml
```

The `dune-project` plaintext file at the top level tells dune how to build your project. The file `src/dune` tells dune the name of the library (`CatML`) and the fact that we have a ocamllex file `lexer.mll` and a menhir parser specification file `parser.mly` in the `src` subdirectory.

```
(library (name CatML))
(menhir (modules parser))
(ocamllex lexer)
```

The files in the `src` directory include:

- `lang.ml`: The main definition of the CatML language, including syntax, evaluation, and type checking
- `lexer.mll`: The specification of the lexer used by ocamllex
- `parser.mly`: The specification of the parser used by menhir
- `main.ml`: Demonstration of how to access the lexer and parser generated from the above files


## Other Resources

1. Parsing and lexing generators in OCaml, [OCaml Programming, Correct + Efficient + Beautiful](https://cs3110.github.io/textbook/chapters/interp/parsing.html)
2. Parsing with OCamllex and Menhir, [Real World OCaml](https://dev.realworldocaml.org/parsing-with-ocamllex-and-menhir.html)
3. [Dune build system reference](https://github.com/ocaml/dune)

## How to interact with a CatML program

1. the first step is to build the project from the repo root and then launch it using build utop which looks like this:

dune build  
dune utop  

2. Once you are inside utop, open CatML and Main:

open CatML;;  
open Main;;  

3. You can then parse strings or run .catml files using run_catML_file (here is an example of a factorial function that should print 120):

run_catML_file "tests/fact.catml";;


# Reflection Questions

1. If you worked with a team, list all the members of the team and briefly state what contributions each person made.

I worked alone on this assignment.

2. Did you use generative AI to assist you in implementing this project? If so, briefly explain how you used it and for which components.

Yes I used Gemini to help me understand how to setup dune properly as I was having problems with dune not building and I ended up having to reinstall ocaml and run dune/ocaml on linux instead of windows. AI was also used to help me figure out how ocamllex and menhir tokens connect to main.ml (Lexer.read and Parser.prog), catch Failure and Parser.Error cleanly in run_catML_file, and format clear error messages and documentation.

3. Did you make any design decisions that we should know about?

Due to the absence of explicit type annotations in Fun and Fix within the AST, attempting to look up bound parameters or recursive function names directly in empty_context would result in failures during the type-checking of closed expressions. I modified the type_of_expr function in src/lang.ml to employ fresh type variables and unification (infer/unify) prior to converting back to arith_type. This adjustment allows Fun, Fix, and App, as well as recursive functions like factorial, to type-check from empty_context. Additionally, I kept the parse function in src/main.ml and introduced run_catML_file with string_of_val formatter.

4. Did you implement any of the extra credit features? Briefly summarize your work.

I added type inferencing using unify and infer in src/lang.ml using fresh type vars so that Fun, Fix and App expressions can type check from empty_context without explicit type annotations.

5. Any feedback on the structure of this assignment?

I would much prefer to have smaller weekly homeworks. If being in a group is an option, then I think that people should either be put into premade groups or not have them at all. 

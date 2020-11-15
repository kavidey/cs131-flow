# The _flow_ language

This repository contains a reference implementation of the _flow_ language.

## Requirements

To run the code, it's best to use the CS 131 Docker image. If you want to run it on your
own machine, here is a list of the libraries you will need:

+ [`parsec`](https://hackage.haskell.org/package/parsec) (for parsing)
+ [`cmdargs`](https://hackage.haskell.org/package/cmdargs) (for processing command-line arguments)

## Running programs

You can run a _flow_ program with the following command:

```
runhaskell flow.hs <path-to-program>
```

For example, to run the program `example-programs/small.flow`, use the command:

```
runhaskell flow.hs example-programs/small.flow
```

_(You can also compile the program with `ghc -O flow.hs`, then use `./flow` instead of
`runhaskell flow.hs`)_

### Showing the final store

You can ask `flow` to show a program's final store with the `-s` option, as in:

```
runhaskell flow.hs -s example-programs/small.flow
```

## Creating a dot representation of a program

You can create a `dot` representation of the program using `flow`'s `dot` mode, like so:

```
runhaskell flow.hs dot example-programs/small.flow
```

This command will print the `dot` representation to standard out.

### Creating pictures from programs

If you are working in the CS 131 Docker image, you can pipe the output of `flow dot` to
the `dot` program:

```
runhaskell flow.hs dot example-programs/small.flow | dot -Tpng > out.png
```

This command will create a file `out.png` that contains a picture of the program's
control-flow graph. You should be able to open `out.png` on your computer, or in Visual
Studio Code.

## The `flow` program

In general, you can find information about how to run _flow_ programs by running the command:

```
runhaskell flow.hs --help
```

which gives the following output:
```
The flow program

flow [COMMAND] ... [OPTIONS]
  The flow programming language

Common flags:
  -? --help        Display help message
  -V --version     Print version information

flow [run] [OPTIONS] [PROGRAM]
  Run the program

  -s --show-store

flow dot [OPTIONS] [PROGRAM]
  Render the program in dot notation
```

## Extending the implementation

If you want to extend the implementation, to add an analysis, here are some
recommendations:

+ Add the code for the analysis in the `Semantics` folder.
+ Take inspiration from the concrete semantics in `Semantics/ConcreteSemantics.hs`.
+ The file `Semantics/Domains.hs` has some helper functions and data structures for stores
  and lattices.
+ You can add the analysis to the `flow` program, by adding an `analysis` mode. It's
  probably good to ask for help on this.
+ If you want to add a program transformation that uses the results of your analysis, you
  probably want to create a new module and add a `transform` mode to the `flow` program.

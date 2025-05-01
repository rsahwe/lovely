# The Lovely Programming Language

A purpose-built programming language for developing humbleOS

# Steps
1. lex
2. parse
3. typecheck
4. asm gen

# Next steps
- [ ] parse what we can lex <-
- [ ] tree-sitter grammar

## Core philosophies
1. if readability isn't lessened, less syntax is better

## Features (from discord)
- inline assembly
- no tabs
- strong type system
- compiled
- fast compilation
- good enums w/ associated values
- first-class functions
- open source ✅
- a video series ✅
- interfaces/traits/protocols
- pattern matching
- algebraic data types
- anonymous sum and product types
- labeled tuples
- immutability by default
- compile time evaluation
- typed errors
- stack trace stuff
- custom operators

## To be debated:
- C-style squirrelies

## Syntax

```
## Lovely

# variable declaration
foo :: 4; # constant
bar : Int = 4; # mutable

# functions
calc :: fun (~x, ~y: Int) Int {
  z :: x / y;
  z^2
};

calc(foo, bar);

add :: fun (~num: Int, to other_num: Int) {
  num + other_num
};

add(3, to: 4);
```

## Language:

- rust
- ocaml

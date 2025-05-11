# Language
- [/] lex
- [/] parse
- [ ] typecheck <- 🕺
- [ ] asm gen

# Tools
- [/] tree-sitter parser
- [ ] language server
- [ ] formatter
- [ ] package manager

# Next steps
- [x] parse what we can lex <-
- [x] tree-sitter grammar

## Core philosophies
1. if readability isn't lessened, less syntax is better

## Features (from discord)
- inline assembly
- no tabs ✅
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
- field traits

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

## User-defined types

```lovely
# everything is an enum (lovel-enums)

type Expression {
  @span : Span;
  
  Literal(Int),
  Variable(String),
  Call({
    @name : String;
    @args : List<Expression>;
  }),
}

type Species {
  Dog,
  Cat,
  Fish,
  Other(String),
}

type Animal {
  @size      : Int;
  @does_bite : Bool;
  @can_bite  : Bool;
  @species   : Species;
};

foo := Animal {
  size      = 4,
  does_bite = true,
  can_bite  = false,
  species   = Species.Other("hamster"),
};

fry :: fun (~s: Species) {
  print("Frying up a \(Species.@to_string())")
};
fry(.Fish);

expr := Expression.Call({
  name = "foo",
  args = [],
}) {
  span = ...
};
```

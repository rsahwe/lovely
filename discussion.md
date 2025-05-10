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

Expression :: {
  @span: Span,
  
  literal(Int),
  variable(String),
  call({
   @name: String,
   @args: List<Expression>,
  }),
}

Species :: {
  dog,
  cat,
  fish,
  other(String),
}

Animal :: {
  @size: Int,
  @does_bite: Bool,
  @can_bite: Bool,
  @species: Species,
};

foo := Animal {
  size      = 4,
  does_bite = true,
  can_bite  = false,
  species   = Species.other("hamster"),
};

fry :: fun (~s: Species) {
  print("Frying up a \(Species@to_string())")
}
fry(.fish)

expr := Expression.call({
  name = "foo",
  args = [],
}) {
  span = ...
};
```

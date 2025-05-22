# The Lovely Programming Language

Lovely is a low-level systems programming language, designed to be used to create the Humble operating system.

## Contributing:

All language features are being developed live, on my [YouTube channel](https://www.youtube.com/@kiahjh-dev); however, if you have any optimizations/enhancements/fixes you want to contribute, please feel free to open a PR! I'm not currently accepting PRs that add new features/functionality to the language, because this is a learning project and I'd like to give those things a shot myself.

## The language

### Variable declarations

Mutable variables are declared using `:=`, and immutable ones with `::`:

```lovely
foo := 4; # mutable var
bar :: 2; # immutable var
```

Types are inferred, but can be specified like so:

```lovely
foo : Int = 4;
bar : Int : 2;
```

If you specify the type, you can declare and initialize a variable on two different lines:

```lovely
# declare foo
foo : Int;

# later...
foo = 4;
```

### Comments

Comments begin with `#` and continue to the end of the line.

### Types

Lovely supports the following primitive data types:

- `Int`: a signed int of size TODO
- `Unit`: equivalent to `void` or `()` in some other languages
- `Bool`: `true` or `false`

### Operators

Lovely supports the following operators:

Arithmentic:
- `+`: addition
- `-`: minus if infix, negative if prefix
- `*`: multiplication
- `/`: division

Comparative:
- `==`: equality
- `!=`: inequality
- `<`: less than
- `>`: greater than
- `<=`: less than or equal
- `>=`: greater than or equal

Logical operators:
- `!`: negates a boolean

Bitwise operators:
- `&`: bitwise and
- `|`: bitwise or
- `^`: bitwise xor
- `~`: bitwise negation

Parentheses can be used to group expressions:

```lovely
3 * (4 + 5)
```

### Function expressions

Function expressions use the following format:

```lovely
fun (<parameters>) <return_type> { <body> }
```

Function parameters can optionally have two labels, one for use inside the function implementation, and one for the callsite; if you only use one label, it will be used in both places. If you don't want to have to provide a label at the callsite, prefix the label with `~`.

```lovely
add :: fun (~first: Int, to second: Int) Int {
  first + second
};

add(3, to: 4);
```

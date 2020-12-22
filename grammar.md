# Grammar

## Core language

```
# Line comments

# Primitives
true                         # booleans
false
23                           # integers
133.7                        # floats
'a'                          # characters
"Hello, world!"              # strings

# Composite types
("Elon", 1337)               # tuples
['e', 'l', 'o', 'n']         # lists

record Coord =
  { x: float,
    y: float,
    z: float,
  }

Coord { x: 238.4, y: 45.0, z: 931.43 }  # records

variant Destination =
  | Earth
  | Mars
  | Planet({ name: string; coord: Coord })

Destination::Mars            # variants

# Compound expressions
let a = 5                    # let expression
let b: float = 42.0          # optional type annotation
Int.to_float(a) + b          #=> 47.0

match (a mod 3, a mod 5)     # match expression
| (1, 0) -> "fizz"           # match case
| (0, 1) -> "buzz"
| (1, 1) -> "fizzbuzz"
| _ -> Int.to_string(a)      # wildcard / catch-all case

let coolness =
  if a > 9000 then           # conditional expression
    "Over 9000"              # indentation-based scoping
  else                       # optional-else branch
    "Meh"

coolness                     #=> "Meh"

# single-line conditional expression
let cool = if a > 9000 then "Over 9000" else "Meh"

# Functions
let hypotenuse(a: float, b: float) -> float =
  Float.sqrt(a ** 2 + b ** 2)

# Currying

# Function can have multiple parameter lists to facilitate currying
let add(x: int)(y: int) -> int = x + y

# Applying all arguments evaluates the function
add(1)(2)
# Partially applying returns a function that takes the remaining arguments
let bump: (int) -> int = add(1)
# Functions are values and can be passed around (ie first-class functions)
List.map(bump)([1, 2, 3, 4, 5])  #=> [2, 3, 4, 5, 6]

# Closures
# Non-top level functions do not require type-annotations
let multiplier = 2
List.map((x) => x * multiplier)([1, 3, 5, 7, 8])  #=> [1, 9, 25, 49, 64]
```

## Indentation

```
# Expression body must either be on the same line as its parent or
# start from a newline with one indentation.
let _ =
  if cond then
    # then branch
  else
    # else branch

let _ = if cond
  value()      # error

# multiline expression must have subsequent lines indented

let compiler(input: string) =
  input
    |> lex
    |> parse
    |> codegen

f(arg1)
  (arg2, arg3)

# indentation within paranthesis

let value = take_a_lot_of_args(
  arg1,
  (x) => x / 42,
  [1, 2, 3,
   4, 5, 6],
)(
  arg4,
  (x)(y) =>
    x
      + y
)
```

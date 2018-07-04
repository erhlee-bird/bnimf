# TinyC

# Decide what EBNF grammar to support.
# Should repetitions be described with wrapped curly braces or regex-like *.
program: { statement } # program: statement *

statement:
    "if" paren_expr statement
  | "if" paren_expr statement "else" statement
  | "while" paren_expr statement
  | "do" statement "while" paren_expr ";"
  | "{" { statement } "}"
  | expr ";"
  | ";"

paren_expr:
  "(" expr ")"

expr:
    test
  | id "=" expr

test:
    sum
  | sum "<" sum

sum:
    term
  | sum "+" term
  | sum "-" term

term:
    id
  | integer
  | paren_expr

id: STRING

integer: INT

STRING:
  { "a" | "b" | "c" | "d" | "e" | "f" | "g" | "h" | "i" | "j" | "k" | "l"
  | "m" | "n" | "o" | "p" | "q" | "r" | "s" | "t" | "u" | "v" | "w" | "x"
  | "y" | "z" }

INT:
  { "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" }

WS: " " | "\r" | "\n" | "\t"

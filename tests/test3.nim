import bnimf

# Context-free languages
# Lua, Python, Lisps

# Context-free grammars are different from right regular grammars by allowing
# literals on both sides.

# Pass
#
# ()
# (())
# ((()))

rule matched_parens: "" | "(" matched_parens ")"

# S = ''
# S = '(' S ')'

# Use Lua Grammar for another example.

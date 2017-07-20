##[
Create a DSL for generating EBNF-based parsers.

e.g.

# The base language of the DSL must adhere to Nim grammar.
# The symbols cannot have `-` characters as they are invalid for forming Nim
# ident nodes.

# The DSL works by
# 1) Consuming a set of NimNodes.
# 2) Maintaining a dependency graph of the rules.

rule rule_name: ident | number

# Terminals that appear on the right should be peg strings following the
# specification present in the Nim stdlib pegs module.

rule ident: "\a\w+"

rule number: "\d+"
]##
import macros, pegs, tables
import ./graph

type
  RuleException = object of Exception

var
  symbols: Graph[string] = newGraph[string]()
    ## For every lhs rule, add the rule as a node to the symbols graph.
    ## For every rhs symbol, add an edge from the rule to the symbol if and only
    ## if the symbol is not already present as a node.
    ##
    ## At the end of parsing, iterate over the edges of the graph and remove any
    ## where both nodes are present in the graph.
    ##
    ## If there are edges remaining that do not have both nodes present, we have
    ## an unmet dependency and an appropriate error can be raised.

  parses: Table[string, seq[string]]
  terminals: seq[string] = @[]

## Operators of the EBNF Language.
##
## definition =
## concatenation ,
## termination ;
## alternation |
## optional [ ... ]
## repetition { ... }
## grouping ( ... )
## terminal string " ... "
## terminal string ' ... '
## comment (* ... *)
## special sequence ? ... ?
## exception -

proc collapseOp(op: NimNode): NimNode =
  case op.kind
  of nnkIdent:
    let opStr = $op.ident
    return (quote do:
      `opStr`.echo)
  else:
    raise newException(RuleException,
                       "Invalid operator provided in rule definition.")
  result = op

proc collapse(parent: NimNode, rule: string): NimNode =
  ## Take the invalid configuration NimNodes and generate more appropriate nodes
  ## that will serve to generate the lexer/parser/etc.
  result = newStmtList(@[])
  case parent.kind
  of nnkIdent:
    let parentNode = $parent.ident
    # Add the new symbol as a node to the graph.
    # Add an edge from the rule to the symbol to form a new dependency.
    result.add(quote do:
      symbols.addNodeTry(`parentNode`)
      symbols.addEdge(`rule`, `parentNode`))
  of nnkCommand:
    # Lisp-like command application for lists of symbols.
    # Collapse the command and add the symbols to the lookup table.
    for node in parent:
      node.collapse(rule).copyChildrenTo(result)
  of nnkInfix:
    # Display a warning, this most likely signifies that symbols were written
    # with dashes in the names.
    var op = parent[0]
    op.ident.echo
    result = newEmptyNode()
  else:
    discard

macro rule(name: untyped, body: untyped): typed =
  ## Collapse the body statements into a sequence of symbols and terminals.
  if body.len != 1:
    raise newException(RuleException,
                       "Invalid rule syntax: 'rule name: part1 part2 | part3'.")
  result = newStmtList(@[])
  let ruleNode = $name
  # Add the rule as a node to the symbol graph.
  # Find any nodes that have an outgoing edge to us and delete it.
  result.add(quote do:
    symbols.addNodeTry(`ruleNode`)
    for node in symbols.nodes:
      symbols.delEdge(node, `ruleNode`))
  for node in body:
    node.collapse($name).copyChildrenTo(result)

## Tests
when isMainModule:
  dumpTree:
    rule test: a b | c | d

  rule rule_expr: rule_name | rule_number
  rule rule_name: ident
  rule rule_number: number
  rule ident: peg"\ident"

  echo "BEGIN"
  for edge in symbols.edges:
    echo edge[0] & " -> " & edge[1]
  echo "END"

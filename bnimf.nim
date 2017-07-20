##[
Create a DSL for generating EBNF-based parsers.

Operators of the EBNF Language.

definition =
concatenation ,
termination ;
alternation |
optional [ ... ]
repetition { ... }
grouping ( ... )
terminal string " ... "
terminal string ' ... '
comment (* ... *)
special sequence ? ... ?
exception -

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
import macros, pegs, strutils, tables
import ./graph

type
  RuleException = object of Exception

  AstKind = enum
    ast_rule_list,
    ast_rule,
    ast_parse,
    ast_rule_ref,

  AstNode = object
    children: seq[AstNode]
    case kind: AstKind
    of ast_rule, ast_rule_ref:
      ident: string
    else:
      discard

proc `$`(A: AstNode): string =
  let kindstr = ($A.kind).replace("ast_", "")
  case A.kind
  of ast_rule, ast_rule_ref:
    result = kindstr & ": " & A.ident
  else:
    result = kindstr

proc echo(A: AstNode, depth: int = 0) =
  echo("  ".repeat(depth) & $A)
  for child in A.children:
    echo(child, depth + 1)

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

  rules: AstNode = AstNode(kind: ast_rule_list, children: @[])
    ## A list of the collected rules.

  new_rule: AstNode
    ## A global variable to allow the macros to operate disjointly.

  new_parse: AstNode
    ## A global variable to allow the macros to operate disjointly.

proc collapse(parent: NimNode): NimNode =
  ## Collapse the AST into a list of leaf nodes.
  result = newStmtList()
  if parent.len > 0:
    case parent.kind
    of nnkInfix:
      # Infix has the first two idents swapped.
      result.add(parent[1])
      result.add(parent[0])
      parent.del(n = 2)
    else:
      discard
    for child in parent:
      child.collapse.copyChildrenTo(result)
  else:
    result.add(parent)

macro rule(name: untyped, body: untyped): typed =
  ## Collapse the body statements into a sequence of symbols and terminals.
  result = newStmtList()
  let strname = $name
  # Create a new rule node.
  result.add(quote do:
    new_rule = AstNode(kind: ast_rule, ident: `strname`, children: @[]))
  # Create a new parse node.
  result.add(quote do:
    new_parse = AstNode(kind: ast_parse, children: @[]))
  # With the flattened list of AST leaf nodes, generate code to separate them
  # into separate parses.
  for node in body.collapse:
    case node.kind
    of nnkIdent:
      let strid = $node.ident
      if strid == "|":
        # Handle alternation.
        result.add(quote do:
          new_rule.children.add(new_parse)
          new_parse = AstNode(kind: ast_parse, children: @[]))
      else:
        result.add(quote do:
          new_parse.children.add(AstNode(kind: ast_rule_ref, ident: `strid`)))
    else:
      raise newException(RuleException, "Unexpected node type: " & $node.kind)
  result.add(quote do:
    rules.children.add(new_rule))

proc dependency_graph() =
  discard

proc generate() =
  ## Call this function after all rules have been define.
  ##
  ## 1) A dependency graph will be generated to ensure all requisite rules are
  ##    defined.
  dependency_graph()

## Tests
when isMainModule:
  rule rule_expr: rule_name | rule_number

  rule a: b | c d | e

  rules.echo

  echo "BEGIN"
  for edge in symbols.edges:
    echo edge[0] & " -> " & edge[1]
  echo "END"

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
import macros, pegs, sequtils, strutils, tables
import ./graph

type
  RuleException = object of Exception

  AstKind = enum
    ast_rule_list,
    ast_rule,
    ast_parse,
    ast_rule_ref,
    ast_lit,

  AstNode = object
    children: seq[AstNode]
    case kind: AstKind
    of ast_rule, ast_rule_ref:
      ident: string
    of ast_lit:
      pattern: string
    else:
      discard

proc `$`(A: AstNode): string =
  let kindstr = ($A.kind).replace("ast_", "")
  case A.kind
  of ast_rule, ast_rule_ref:
    result = kindstr & ": " & A.ident
  of ast_lit:
    result = kindstr & ": \"" & A.pattern & "\""
  else:
    result = kindstr

proc echo(A: AstNode, depth: int = 0) =
  echo("  ".repeat(depth) & $A)
  for child in A.children:
    echo(child, depth + 1)

var
  rules: AstNode = AstNode(kind: ast_rule_list, children: @[])
    ## A list of the collected rules.

  dep_graph: Graph[string] = newGraph[string]()
    ## A dependency graph for the rules.

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
    of nnkStrLit, nnkRStrLit:
      let strval = $node.strVal
      result.add(quote do:
        new_parse.children.add(AstNode(kind: ast_lit, pattern: `strval`)))
    else:
      raise newException(RuleException, "Unexpected node type: " & $node.kind)
  result.add(quote do:
    new_rule.children.add(new_parse)
    rules.children.add(new_rule))

# Routines to run post rule generation.

proc static_check_pass(node: AstNode) {.raises: [RuleException].} =
  ## Verify that the provided AstNode is properly typed.
  proc all_children_of(node: AstNode, ckind: seq[AstKind]) =
    # All children of the provided `node` must be of kind `ckind`.
    if not allIt(node.children, it.kind in ckind):
      var astrepr = ("Expected all children of " & $node & " to be of type " &
        $ckind & ".\n")
      for child in node.children:
        astrepr &= "  " & $child
      raise newException(RuleException, astrepr)
  case node.kind
  of ast_rule_list:
    # All children should be of kind ast_rule.
    node.all_children_of(@[ast_rule])
  of ast_rule:
    # All children should be of kind ast_parse.
    node.all_children_of(@[ast_parse])
  of ast_parse:
    # Children can be of kinds ast_rule_ref.
    node.all_children_of(@[ast_rule_ref, ast_lit])
  of ast_rule_ref, ast_lit:
    discard
  else:
    raise newException(RuleException, "Unhandled node kind: " & $node.kind)
  for child in node.children:
    child.static_check_pass

proc dependency_graph_pass(node: AstNode) =
  ## Create a dependency graph to check if there are any unsatisfied rules.
  case node.kind
  of ast_rule:
    # Capture the current rule we are creating dependencies for.
    new_rule = node
    # Upon finding a rule, add a node representing it to the graph and find any
    # edges that point to the rule and delete them.
    dep_graph.addNodeTry(node.ident)
    for edge in dep_graph.edges(node.ident, false):
      dep_graph.delEdge2(node.ident, edge[1])
  of ast_rule_ref:
    # Add the new symbol that our rule depends on.
    dep_graph.addNodeTry(node.ident)
    # Add the edge modeling the dependency from the current rule to the new one.
    dep_graph.addEdge(new_rule.ident, node.ident)
  else:
    discard
  for child in node.children:
    child.dependency_graph_pass

proc check_rules*() =
  ## Call this function after all rules have been defined.
  ##
  ## 0) Run a pass over the AST to ensure that the proper node types are found
  ##    and a proper tree has been formed.
  ##
  ## 1) A dependency graph will be generated to ensure all requisite rules are
  ##    defined.
  static_check_pass(rules)
  dependency_graph_pass(rules)

## Tests
when isMainModule:
  rule rule_expr: rule_name | rule_number

  rule a: b | c d | e
  rule b: r"\ident"

  dumpTree:
    rule c: "fn"

  rule c: "fn"

  rules.echo

  check_rules()

  echo "BEGIN"
  for edge in dep_graph.edges:
    echo edge[0] & " -> " & edge[1]
  echo "END"

# Stdlib Imports.
import macros, strutils

type
  RuleException* = object of Exception

  AstKind* = enum
    ast_rule_list,
    ast_rule,
    ast_parse,
    ast_rule_ref,
    ast_lit,

  AstNode* = object
    children*: seq[AstNode]
    case kind*: AstKind
    of ast_rule, ast_rule_ref:
      ident*: string
    of ast_lit:
      pattern*: string
    else:
      discard

proc `$`*(A: AstNode): string =
  ## Provides a string representation of an AstNode.
  let kindstr = ($A.kind).replace("ast_", "")
  case A.kind
  of ast_rule, ast_rule_ref:
    result = "(" & kindstr & ") " & A.ident
  of ast_lit:
    result = kindstr & " = \"" & A.pattern & "\""
  else:
    result = kindstr

proc echo*(A: AstNode, depth: int = 0) =
  ## Pretty prints an AstNode.
  echo("  ".repeat(depth) & $A)
  for child in A.children:
    echo(child, depth + 1)

proc collapse*(parent: NimNode): NimNode =
  ## Collapse the AST into a list of leaf nodes.
  result = newStmtList()
  if parent.len > 0:
    case parent.kind
    of nnkInfix:
      # Infix has the first two idents swapped.
      # e.g.
      #   Infix(Ident(ident"|"),
      #         Ident(ident"rule_name"),
      #         Ident(ident"rule_number"))
      #
      #   becomes
      #
      #   StmtList(Ident(ident"rule_name"),
      #            Ident(ident"|"),
      #            Ident(ident"rule_number"))
      result.add(parent[1])
      result.add(parent[0])
      parent.del(n = 2)
    else:
      # Any node type aside from Infix can remain in order.
      discard
    for child in parent:
      child.collapse.copyChildrenTo(result)
  else:
    result.add(parent)

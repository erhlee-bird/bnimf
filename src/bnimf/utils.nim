# Stdlib Imports.
import macros
# 3rd-party Imports.
import simple_graph

template addNodeTry*(G, A: untyped): untyped =
  ## Attempt to add a node regardless of whether or not it exists.
  try:
    G.addNode(A)
  except KeyError:
    discard

template delEdge2*(G, A, B: untyped): untyped =
  ## Delete edges in both directions for DirectedGraphs.
  G.delEdge(A, B)
  G.delEdge(B, A)

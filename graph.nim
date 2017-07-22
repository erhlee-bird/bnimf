import intsets, tables

type
  Node = int

  GraphAssign = object
    id: Node
      ## Provide the next available node id.
    reserve: seq[Node]
      ## Maintain a sequence of newly freed ids for reuse.

  GraphMap[T] = object
    key_to_node: Table[T, Node]
      ## Provide a lookup from a key to a node id.
    node_to_key: Table[Node, T]
      ## Provide a reverse lookup from a node id to a key.

  GraphSet = object
    nodes: IntSet
    outEdges: Table[Node, IntSet]
    inEdges: Table[Node, IntSet]

  Graph*[T] = object
    assign: GraphAssign
    map: GraphMap[T]
    graph: GraphSet

proc newGraphAssign(): GraphAssign =
  result.id = 0
  result.reserve = @[]

proc next(GA: var GraphAssign): Node =
  ## Get the next available node id.
  if GA.reserve.len > 0:
    return GA.reserve.pop()
  result = GA.id
  GA.id += 1

proc newGraphMap[T](): GraphMap[T] =
  result.key_to_node = initTable[T, Node]()
  result.node_to_key = initTable[Node, T]()

proc add[T](GM: var GraphMap, A: T, n: Node) =
  GM.key_to_node[A] = n
  GM.node_to_key[n] = A

proc newGraphSet(): GraphSet =
  result.nodes = initIntSet()
  result.outEdges = initTable[Node, IntSet]()
  result.inEdges = initTable[Node, IntSet]()

proc add(GS: var GraphSet, n: Node) =
  GS.nodes.incl(n)
  GS.outEdges[n] = initIntSet()
  GS.inEdges[n] = initIntSet()

proc add(GS: var GraphSet, f: Node, t: Node) =
  GS.outEdges[f].incl(t)
  GS.inEdges[t].incl(f)

proc newGraph*[T](): Graph[T] =
  result.assign = newGraphAssign()
  result.map = newGraphMap[T]()
  result.graph = newGraphSet()

proc `[]`*[T](G: Graph[T], A: T): Node {.raises: [KeyError].} =
  ## Lookup a graph node.
  result = G.map.key_to_node[A]

iterator edges*[T](G: Graph[T], A: T, out_edges: bool = true): array[0..1, T]
    {.raises: [KeyError].} =
  let collection = if out_edges:
                     G.graph.outEdges[G[A]]
                   else:
                     G.graph.inEdges[G[A]]
  for node in collection.items:
    yield [A, G.map.node_to_key[node]]

iterator edges*[T](G: Graph[T], out_edges: bool = true): array[0..1, T]
    {.raises: [KeyError].} =
  for key in G.map.key_to_node.keys:
    for edge in G.edges(key, out_edges):
      yield edge

proc add[T](G: var Graph[T], A: T, B: T) {.raises: [KeyError].} =
  ## Add a new edge to the graph.
  G.graph.add(G[A], G[B])

proc del[T](G: var Graph[T], A: T, B: T) {.raises: [KeyError].} =
  ## Delete an edge from the graph.
  G.graph.outEdges[G[A]].excl(G[B])
  G.graph.inEdges[G[B]].excl(G[A])

template addEdge*[T](G: var Graph[T], A: T, B: T) =
  G.add(A, B)

template addEdge2*[T](G: var Graph[T], A: T, B: T) =
  G.add(A, B)
  G.add(B, A)

template delEdge*[T](G: var Graph[T], A: T, B: T) =
  G.del(A, B)

template delEdge2*[T](G: var Graph[T], A: T, B: T) =
  G.del(A, B)
  G.del(B, A)

iterator nodes*[T](G: Graph[T]): T =
  for key in G.map.key_to_node.keys:
    yield key

proc add[T](G: var Graph[T], A: T) =
  ## Add a new node to the graph.
  if G.map.key_to_node.hasKey(A):
    raise newException(KeyError, "Graph already has node: '" & $A & "'")
  let N = G.assign.next()
  G.map.add(A, N)
  G.graph.add(N)

proc del[T](G: var Graph[T], A: T) {.raises: [KeyError].} =
  ## Delete a node from the graph.
  let nodeid = G.map.key_to_node[A]
  # Release the nodeid.
  G.assign.reserve.add(nodeid)
  # Delete edges in the graph and then the node.
  for edge in G.edges(A):
    G.delEdge2(A, edge)
  G.graph.nodes.excl(nodeid)
  G.graph.outEdges.del(nodeid)
  G.graph.inEdges.del(nodeid)
  # Unmap the node.
  G.map.key_to_node.del(A)
  G.map.node_to_key.del(nodeid)

template addNode*[T](G: var Graph[T], A: T) =
  G.add(A)

template addNodeTry*[T](G: var Graph[T], A: T) =
  try: G.add(A) except: discard

template delNode*[T](G: var Graph[T], A: T) =
  G.del(A)

## Tests
when isMainModule:
  var G: Graph[char] = newGraph[char]()

  G.addNode('a')
  G.addNode('b')
  G.addNode('c')
  # ID Starts at 0.
  assert(G['a'] == 0)
  # Deleting a node will cause replacement.
  G.delNode('b')
  G.addNode('b')
  assert(G['b'] == 1)
  # Add edges.
  G.add('a', 'b')
  G.addEdge('b', 'a')
  G.add('a', 'c')

  for node in G.nodes:
    echo("Node: ", node)
    for edge in G.edges(node):
      echo("\tEdge: ", edge)

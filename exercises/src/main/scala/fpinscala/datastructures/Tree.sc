import fpinscala.datastructures._

val t = Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))

Tree.size(t)
Tree.size_f(t)

Tree.maximum(t)
Tree.maximum_f(t)

Tree.depth(t)
Tree.depth_f(t)

Tree.map(t)(_ * 2)
Tree.map_f(t)(_ * 2)

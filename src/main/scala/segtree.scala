package segtree

// Node and Leaf
abstract class Tree[T]

// Internal Node
case class Node[T](left: Tree[T], right: Tree[T], l: Int, r: Int, value: T) extends Tree[T]

// Leaf
case class Leaf[T](idx: Int, value: T) extends Tree[T]

// Immutable Segment Tree
object SegmentTree {
  private def mid(l: Int, r: Int): Int = (l + r) / 2

  // @param l: left index
  // @param r: right index
  // @param default: default value of the node
  // @return: root of the tree
  def build[T](l: Int, r: Int, default: T): Tree[T] = {
    if (l != r) Node(build(l, mid(l, r), default), build(mid(l, r) + 1, r, default), l, r, default)
    else Leaf(l, default)
  }

  /*
    @param root: root of the tree
    @param idx: index of the value to be updated
    @param value: new value
    @param g: function to update the value of the node, aggregator
    @return: new tree and the value of the node
   */
  def insert[T](root: Tree[T], idx: Int, value: T, g: (T, T) => T): (Tree[T], T) =
    root match {
      case Node(left, right, l, r, v) =>
        if (idx <= mid(l, r)) {
          val (newLeft, newV) = insert(left, idx, value, g)
          (Node(newLeft, right, l, r, g(newV, v)), newV)
        } else {
          val (newRight, newV) = insert(right, idx, value, g)
          (Node(left, newRight, l, r, g(v, newV)), newV)
        }
      case Leaf(i, v) => (Leaf(i, value), value)
    }

  // @return: value of the node
  def query[T](root: Tree[T], l: Int, r: Int, g: (T, T) => T): T =
    root match {
      case Node(left, right, rootL, rootR, v) =>
        if (l == rootL && r == rootR) v
        else if (r <= mid(rootL, rootR)) query(left, l, r, g)
        else if (l > mid(rootL, rootR)) query(right, l, r, g)
        else g(query(left, l, mid(rootL, rootR), g), query(right, mid(rootL, rootR) + 1, r, g))
      case Leaf(i, v) => v
    }
}

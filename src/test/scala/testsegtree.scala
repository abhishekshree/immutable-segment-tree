package segtree

import SegmentTree._

class TestSegmentTree extends munit.FunSuite {
  test("test build") {
    val root = build(0, 3, 0)
    assert(root == Node(Node(Leaf(0, 0), Leaf(1, 0), 0, 1, 0), Node(Leaf(2, 0), Leaf(3, 0), 2, 3, 0), 0, 3, 0))
  }

  test("test insert") {
    val root = build(0, 3, 0)
    val (newRoot, value) = insert(root, 1, 1, (x: Int, y: Int) => x + y)
    assert(newRoot == Node(Node(Leaf(0, 0), Leaf(1, 1), 0, 1, 1), Node(Leaf(2, 0), Leaf(3, 0), 2, 3, 0), 0, 3, 1))
    assert(value == 1)
  }

  test("test query") {
    val root = build(0, 3, 0)
    val (newRoot, value) = insert(root, 1, 1, (x: Int, y: Int) => x + y)
    val result = query(newRoot, 0, 3, (x: Int, y: Int) => x + y)
    assert(result == 1)
  }

  test("range sum") {
    val root = build(0, 3, 0)
    val (newRoot, value) = insert(root, 1, 1, (x: Int, y: Int) => x + y)
    val result = query(newRoot, 0, 3, (x: Int, y: Int) => x + y)
    assert(result == 1)
  }
}

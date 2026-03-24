package ch03

import ch03.Tree.{ Leaf, Node }

class TreeSuite extends munit.FunSuite {
  test("map should transform the values in the tree") {
    val obtained = Tree(Node(Leaf(1), Node(Leaf(2), Leaf(3))))
    val expected = Tree(Node(Leaf("1"), Node(Leaf("2"), Leaf("3"))))
    assertEquals(obtained.map(_.toString), expected)
  }
  
  test("size should return the number of nodes in the tree") {
    val obtained = Tree(Node(Leaf(1), Node(Leaf(2), Leaf(3))))
    val expected = 3
    assertEquals(obtained.size, expected)
  }
  
  test("contains should check if a value is present in the tree") {
    val tree = Tree(Node(Leaf(1), Node(Leaf(2), Leaf(3))))
    assert(tree.contains(2))
    assert(!tree.contains(4))
  }
  
  test("fold should aggregate values in the tree") {
    val obtained = Tree(Node(Leaf(1), Node(Leaf(2), Leaf(3))))
    val expected = 6
    assertEquals(obtained.fold(0)(_ + _), expected)
  }
}

package ch09

import cats.implicits.toFunctorOps

class TreeFunctorSuite extends munit.FunSuite {
  test("Tree: map") {
    val tree = Tree.Branch(
      Tree.Leaf(1),
      Tree.Branch(
        Tree.Leaf(2),
        Tree.Leaf(3)
      )
    )

    val mappedTree = tree.map(_ * 2)

    val expectedTree = Tree.Branch(
      Tree.Leaf(2),
      Tree.Branch(
        Tree.Leaf(4),
        Tree.Leaf(6)
      )
    )

    assertEquals(mappedTree, expectedTree)
  }
}

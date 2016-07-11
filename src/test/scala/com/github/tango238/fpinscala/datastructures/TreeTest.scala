package com.github.tango238.fpinscala.datastructures

import org.specs2.mutable.Specification

class TreeTest extends Specification {

  "TreeTest" should {
    "foldLeft" in {
      Tree.foldLeft(Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(5))), 0)(_ + _) must_== 11
      Tree.foldLeft(Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Branch(Leaf(5), Leaf(7)))), 1)(_ + _) must_== 19
    }

    // TODO `foldLeft` と `foldRight` のテストを差異がわかるようなテストにする
    "foldRight" in {
      Tree.foldRight(Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(5))), 0)(_ + _) must_== 11
      Tree.foldRight(Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Branch(Leaf(5), Leaf(7)))), 1)(_ + _) must_== 19
    }

    "size" in {
        Tree.size(Branch(Branch(Leaf("a"), Leaf("b")), Branch(Leaf("c"), Leaf("d")))) must_== 7
    }

    "maximum" in {
      Tree.maximum(Branch(Branch(Leaf(1), Leaf(11)), Branch(Leaf(2), Branch(Leaf(7), Leaf(5))))) must_== 11
    }
  }
}


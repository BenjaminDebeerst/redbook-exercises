package chapter3

object Exercise3_25_to_3_29 {


  def main(): Unit = {
    val tree = Branch(Branch(Leaf(1), Leaf(7)), Leaf(3))
    println(size(tree))
    println(leaves(tree))
    println(nonLeaves(tree))
    println(max(tree))
  }

  def size[A](t: Tree[A]): Int = {
    t match {
      case Leaf(_) => 1
      case Branch(a, b) => 1 + size(a) + size(b)
    }
  }

  def leaves[A](t: Tree[A]): Int = {
    t match {
      case Leaf(_) => 1
      case Branch(a, b) => leaves(a) + leaves(b)
    }
  }


  def nonLeaves[A](t: Tree[A]): Int = {
    t match {
      case Leaf(_) => 0
      case Branch(a, b) => 1 + nonLeaves(a) + nonLeaves(b)
    }
  }

  def max(t: Tree[Int]): Int = {
    t match {
      case Leaf(e) => e
      case Branch(a, b) => max(a) max max(b)
    }
  }

//  def fold[A, B](t: Tree[A], z: B)(f:((left: Tree[A], right: Tree[A]) => B)): B = ???
}

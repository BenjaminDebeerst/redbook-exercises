package chapter3

object Exercise3_25_to_3_29 {


  def main(): Unit = {
    val tree =
      Branch(
        Branch(
          Leaf(1),
          Branch(
            Branch(
              Leaf(3),
              Branch(
                Leaf(1),
                Leaf(2))),
            Branch(
              Leaf(4),
              Leaf(7)))),
        Leaf(3))

    println(size(tree))
    println(leaves(tree))
    println(nonLeaves(tree))
    println(max(tree))
    println(min(tree))
    println(sum(tree))
    println("--")
    val mappedTree = map[Int, Int](tree, i => i - 1)
    println(sum(tree) - leaves(tree))
    println(sum(mappedTree))
    println(max(tree), max(mappedTree))
    println(min(tree), min(mappedTree))
    println(leaves(tree), leaves(mappedTree))
    println(nonLeaves(tree), nonLeaves(mappedTree))
    println(depth(tree), depth(mappedTree))

  }

  def fold[A, B](t: Tree[A])(combine: (B, B) => B, z: A => B): B = {
    t match {
      case Leaf(a) => z(a)
      case Branch(left, right) => combine(fold(left)(combine, z), fold(right)(combine, z))
    }
  }

  def size[A](t: Tree[A]): Int = fold[A, Int](t)(_ + _ + 1, _ => 1)

  def leaves[A](t: Tree[A]): Int = fold[A, Int](t)(_ + _, _ => 1)

  def nonLeaves[A](t: Tree[A]): Int = fold[A, Int](t)(_ + _ + 1, _ => 0)

  def max(t: Tree[Int]): Int = fold(t)((a: Int, b: Int) => a max b, i => i)

  def min(t: Tree[Int]): Int = fold(t)((a: Int, b: Int) => a min b, i => i)

  def sum(t: Tree[Int]): Int = fold[Int, Int](t)((a,b) => a+b, i => i)

  def depth[A](t: Tree[A]): Int = fold[A, Int](t)((a, b) => (a max b) + 1, _ => 0)

  def map[A, B](t: Tree[A], f: A => B): Tree[B] =
    fold[A, Tree[B]](t)(
      (left, right) => Branch(left, right),
      a => Leaf(f(a))
    )
}

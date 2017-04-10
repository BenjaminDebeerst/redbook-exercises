package chapter3

object Exercise3_7 {

  def main(): Unit = {
    //    println(sum(List(1, 2, 3, 4)))
    //    println(product(List(1, 2, 3, 4)))
    //    println(exceptionalProduct(List(1,2,3,4)))
    println(exceptionalProduct(List(1, 2, 0, 4)))

  }


  def sum(ints: List[Int]): Int = foldRight(ints, 0)(_ + _)

  def product(ds: List[Double]): Double = foldRight(ds, 1.0)(_ * _)

  def foldRight[A, B](l: List[A], z: B)(f: (A, B) => B): B = {
    l match {
      case Nil => z
      case Cons(h, t) => f(h, foldRight(t, z)(f))
    }
  }


  // Exercise 3.7.
  /* Because the recursion is done by foldRight, there is no way to halt pre-emptively
  with functional means and the method signatures as currently given.

  The product function could let (_*_) throw an exception that is not caught by foldRight and catch
  it. This is not really functional though.

  Functional approach: let the multiply function return an optional result. Option.None exits calculations.
   */
  def exceptionalFoldRight[A, B](l: List[A], z: B)(f: (A, B) => Option[B]): Option[B] = {
    l match {
      case Nil => Some(z)
      case Cons(h, t) =>
        exceptionalFoldRight(t, z)(f) match {
          case None => None
          case Some(p) => f(h, p)
        }

    }
  }

  def exceptionalProduct(ds: List[Double]): Double = exceptionalFoldRight(ds, 1.0)(fastMultiply) match {
    case None => 0.0
    case Some(p) => p
  }

  def fastMultiply(a: Double, b: Double): Option[Double] = {
    if (a == 0.0 || b == 0.0) {
      println("Found a zero in the row")
      None
    }
    else {
      println(s"$a * $b = ${a * b}")
      Some(a * b)
    }
  }

}

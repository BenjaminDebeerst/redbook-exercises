package chapter3

object Exercise3_7_to_3_15 {


  def main(): Unit = {
    val lists = List(
      List(1, 2, 3),
      List(4),
      List(5, 6, 7, 8),
      List(9, 10)
    )
    println(flattenNonLinear(lists))
    println(flattenLinear(lists))
  }


  def sum(ints: List[Int]): Int = foldRight(ints, 0)(_ + _)

  def product(ds: List[Double]): Double = foldRight(ds, 1.0)(_ * _)

  def foldRight[A, B](l: List[A], z: B)(f: (A, B) => B): B = {
    l match {
      case Nil => z
      case Cons(h, t) => f(h, foldRight(t, z)(f))
    }
  }

  @annotation.tailrec
  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = {
    l match {
      case Nil => z
      case Cons(h, t) => foldLeft(t, f(z, h))(f)
    }
  }

  def sumLeft(ints: List[Int]): Int = foldLeft(ints, 0)(_ + _)

  def productLeft(ints: List[Double]): Double = foldLeft(ints, 1.0)(_ * _)

  def lengthLeft[A](as: List[A]): Int = foldLeft(as, 0)((count, _) => (count + 1))


  // Exercise 3.7.
  /* Because the recursion is done by foldRight, there is no way to halt pre-emptively
  with functional means and the method signatures as currently given.

  The product function could let (_*_) throw an exception that is not caught by foldRight and catch
  it. This is not really functional though.

  Functional approach: let the multiply function return an optional result. Option.None exits calculations.

  Option B: use lazy evaluation of B, changing the signature of foldRight as well:
            def foldRight[A, B](l: List[A], z: B)(f: (A, => B) => B): B
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

  // Exercise 3.8
  /*
   The output is equal to the input. So the data constructors of List can be expressed as a foldRight expression, they are the same
   */

  // Exercise 3.9
  def length[A](as: List[A]): Int = foldRight(as, 0)((_, i) => i + 1)

  def provokeStackOverflow() = {
    def append(l: List[Int], n: Int): List[Int] = {
      if (n < 1) Nil
      else Cons(1, append(l, n - 1))
    }

    def createBigList(): List[Int] = {

      append(List(), 1000000000)
    }

    sum(createBigList())
  }


  // Exercise 3.12
  def reverse[A](as: List[A]): List[A] = {
    foldRight(as, Nil: List[A])((e, l) => concat(l, Cons(e, Nil)))
  }

  // Exercise 3.13
  //  def reverseRight[A](as: List[A]): List[A] = {
  //    foldLeftInTermsOfFoldRight(as, Nil:List[A])( (t, h) => Cons(h, reverse(t)))
  //  }

  def concat[A](l1: List[A], l2: List[A]): List[A] = {
    l1 match {
      case Cons(a, as) => Cons(a, concat(as, l2))
      case Nil => l2
    }
  }

  //  def foldLeftInTermsOfFoldRight[A, B](l: List[A], z: B)(f: (B, A) => B): B = {
  //    def swapArguments[A, B](f:(B, A) => B): ((A, B) => B) = {
  //      (b, a) => f(a, b)
  //    }
  //
  //    def merge[A, B]( f:(B, A) => B, g:(B, A) => B ): (B, A) => B = {
  //      (b: B, a: A) =>
  //        g(f(b, a), a)
  //    }
  //
  //    val reverser = (e: A , ls: List[A]) => concat(ls, Cons(e, Nil))
  //    val inversed_f = swapArguments(f)
  //
  //    val reversed = foldRight(l, Nil:List[A])( reverser )
  //    foldRight(reversed, z)(swapArguments(f))
  //
  //    // TODO does not compile yet
  //    foldRight(l, z)(merge())
  //  }

  // Exercise 3.14
  def normalAppend[A](l1: List[A], l2: List[A]): List[A] = {
    l1 match {
      case Nil => l2
      case Cons(h, t) => Cons(h, normalAppend(t, l2))
    }
  }

  def appendWithFoldRight[A](l1: List[A], l2: List[A]): List[A] = {
    foldRight(l1, l2)((e, l) => Cons(e, l))
  }

  // Exercise 3.15
  // add print to concat to show (non)linearness
  def flattenNonLinear[A](mountains: List[List[A]]): List[A] = {
    foldLeft[List[A], List[A]](mountains, List())((flattened, nextList) => {
      concat(flattened, nextList)
    })
  }

  def flattenLinear[A](mountains: List[List[A]]): List[A] = {
    foldRight[List[A], List[A]](mountains, List())((nextList, flattened) => {
      concat(nextList, flattened)
    })
  }


}

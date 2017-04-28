package chapter3
import Exercise3_7_to_3_15._

object Exercise3_16_to_3_24 {

  def main(): Unit = {
    val list = List(1, 2, 3, 4)

    println(filter(list, (i: Int) => i%2==1))
    println(filterWithFlatMap(list, (i: Int) => i%2==1))
    println(flatMap(List(1,2,3))(e => List(e,e)))

    println(zipWith(List(1,2,3), List(1,2,3))(_+_))
    println(zipWith(List(1,2,3), List(1,2,3,0))(_+_))
    println(zipWith(List(1,2,3,0), List(1,2,3))(_+_))
  }

  // Exercise 3.16 - 3.18
  def map[A, B](list: List[A])(f: A => B): List[B] = {
    foldRight[A, List[B]](list, List())((e, l) => Cons(f(e), l))
  }

  def add(list: List[Int], number: Int) = map(list)((i: Int) => i + number )
  def toString(list: List[Double]) = map(list)((d: Double) => d.toString )

  // Exercise 3.19
  def filter[A](list: List[A], predicate: A => Boolean): List[A] = {
    list match {
      case Nil => Nil
      case Cons(head, tail) if predicate(head) => filter(tail, predicate)
      case Cons(head,tail) if !predicate(head) => Cons(head, filter(tail, predicate))
    }
  }

  // Exercise 3.20
  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = {
    foldRight(as, List[B]())(
      (a, bs) =>
        concat(f(a), bs)
    )
  }

  // Exercise 3.21
  def filterWithFlatMap[A](list: List[A], predicate: A => Boolean): List[A] = {
    flatMap(list)(a => if(predicate(a)) List() else List(a))
  }

  // Exercise 3.22 & 3.23
  def zipWith[A](l1: List[A], l2: List[A])(f: (A, A) => A): List[A] = {
    (l1, l2) match {
      case (Nil, Nil) => Nil
      case (Nil, Cons(h, t)) => Cons(h, t)
      case (Cons(h, t), Nil) => Cons(h, t)
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1,h2), zipWith(t1, t2)(f))
    }
  }

}

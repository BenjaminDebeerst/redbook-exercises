package chapter3

sealed trait List[+A] {
  override def toString: String = {
    def elems(l: List[A]): String = l match {
      case Nil => ""
      case Cons(h, Nil) => h.toString
      case Cons(h, t) => h.toString + ", " + elems(t)
    }
    "[" + elems(this) + "]"
  }
}
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ints: List[Int]): Int = ints match {
    case Nil => 1
    case Cons(0, _) => 0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def tail[A](as: List[A]) = drop(as, 1)

  def drop[A](l: List[A], n: Int): Option[List[A]] = {
    if(n < 1) Some(l)
    else l match {
      case Nil => None
      case Cons(_, Nil) if n == 1 => Some(Nil)
      case Cons(_, tail) => drop(tail, n-1)
    }
  }

  def dropWhile[A](l: List[A], predicate: A => Boolean): List[A] = {
    l match {
      case Nil => Nil
      case Cons(a, tail) if (predicate(a)) => dropWhile(tail, predicate)
      case _ => l
    }
  }

  def setHead[A](head: A, as: List[A]) = as match {
    case Nil => List(head)
    case Cons(_, t) => Cons(head, t)
  }

  def init[A](l: List[A]) : List[A] = {
    l match {
      case Nil => Nil
      case Cons(a, Nil) => Nil
      case Cons(a, t) => Cons(a, init(t))
    }
  }
}


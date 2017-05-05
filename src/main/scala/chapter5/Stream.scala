package chapter5

sealed trait Stream[+A] {

  def toList(): List[A] = this match {
    case Empty => List()
    case Cons(h, t) => h() :: t().toList()
  }


  def take(n: Int): Stream[A] = this match {
      case Empty => Empty
      case _ if n <= 0 => Empty
      case Cons(h, t) => Cons(h, () => t().take(n - 1))
    }


  def drop(n: Int): Stream[A] = this match {
      case Empty => Empty
      case _ if n <= 0 => this
      case Cons(_, t) => t().drop(n - 1)
    }


  def dropWhile(p: A => Boolean): Stream[A] = this match {
      case Empty => Empty
      case Cons(h, t) if p(h()) => Cons(() => h(), t)
      case Cons(_, t) => t().dropWhile(p)
    }


  def exists(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => p(h()) || t().exists(p)
    case _ => false
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case Empty => z
  }

  def forAll(p: A => Boolean): Boolean =
    foldRight(false)((a,b) => p(a) && b)

  def takeWhile(p: A=> Boolean): Stream[A] =
    foldRight(Empty: Stream[A])((h, t) => if(p(h)) Cons[A](() =>h,() => t) else t)

  def headOption(): Option[A] =
    foldRight(None: Option[A])((h, _) => Some(h))

  def map[B](f: A => B): Stream[B] =
    foldRight(Empty: Stream[B])((h,t) => Cons(() => f(h), () => t))

}


case object Empty extends Stream[Nothing]

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] = {
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
  }

  def constant[A](a: A): Stream[A] =
    //cons(a, constant(a))
    unfold(a){case a => Some((a, a))}

  def from(n: Int): Stream[Int] =
    //cons(n, from(n+1))
    unfold(n){case n => Some((n, n+1))}

  def fib(): Stream[Int] = {
    def fib(n1: Int, n2: Int): Stream[Int] = {
      cons(n1, fib(n2, n1+n2))
    }
    fib(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z) match {
      case None => Empty: Stream[A]
      case Some((a, s)) => cons(a, unfold(s)(f))
    }
  }


}

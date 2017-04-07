package chapter3

object Exercise3_1 {
  def main(): Unit = {
    val x = List(1,2,3,4,5,6) match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      case Cons(h, t) => h + List.sum(t)
      case _ => 101
    }
    println(x)
  }


}

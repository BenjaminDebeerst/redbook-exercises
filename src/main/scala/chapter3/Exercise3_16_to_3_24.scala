package chapter3
import Exercise3_7_to_3_15._

object Exercise3_16_to_3_24 {

  def main(): Unit = {
    val list = List(1, 2, 3, 4)
    println(add(list, 2))
    println(toString(List(3.0, 3.123, 4)))
  }

  // Exercise 3.16 - 3.18
  def map[A, B](list: List[A])(f: A => B): List[B] = {
    foldRight[A, List[B]](list, List())((e, l) => Cons(f(e), l))
  }

  def add(list: List[Int], number: Int) = map(list)((i: Int) => i + number )
  def toString(list: List[Double]) = map(list)((d: Double) => d.toString )

  


}

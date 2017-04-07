package chapter2

object Exercise2_3 {
  def main(): Unit = {
    println(curry(sum)(1)(2))
  }

  def curry[A,B,C](f: (A, B) => C): A => (B => C) =
    (a: A) => f(a, _)


  def sum(a: Int, b: Int) = {
    a + b
  }
}

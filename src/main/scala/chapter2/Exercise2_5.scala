package chapter2

object Exercise2_5 {
  def main(): Unit = {
    println(dashes(4))
    println(countCharacters("myString"))
    println(compose(countCharacters, dashes)(4))
  }

  def compose[A,B,C](f: B => C, g: A => B): A => C =
    a => f(g(a))

  def dashes(n: Int): String = List.fill(n)("--").reduce(_ + _)

  def countCharacters(s: String): Int = s.length()
}

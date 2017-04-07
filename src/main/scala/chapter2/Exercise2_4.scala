package chapter2

object Exercise2_4 {
  def main(): Unit = {
    println(multiplyWith(5)(4))

    println(uncurry(multiplyWith)(3,4))
  }

  def uncurry[A,B,C](f: A => B => C): (A, B) => C = {
    (a, b) => f(a)(b)
  }

  def multiplyWith(a: Int): (Int => Int) = {
    b => a * b
  }

}

object Exercise2_1 {
  def main(): Unit = {
    println(fib(1))
    println(fib(2))
    println(fib(3))
    println(fib(4))
    println(fib(5))
    println(fib(6))
    println(fib(7))
  }

  def fib(n: Int) : Int = {
    if (n < 3) 1
    else fib(n-1) + fib(n-2)
  }

  // tail recursive version? TODO
}

package chapter5

object Exercises {
  def main(): Unit = {
    val stream1: Stream[Int] = Cons(() => 1, () => Cons(() => {
      println("2");
      2
    }, () => Stream(3, 4, 5)))
    println(stream1.take(3).toList())

    println(stream1.drop(2).toList())

    println(stream1.dropWhile(_ % 2 == 1))
    println(stream1.dropWhile(_ % 2 == 0))
    println(stream1.dropWhile(_ % 2 == 0))

    println("=====")

    println(stream1.forAll(_ < 1))
    println(stream1.forAll(_ > 0))

    println("-----")
    val stream2 = Stream(1, 2, 3, 4, 5, 6)
    println(stream2.takeWhile(_ % 2 == 0).toList())
    println(stream1.takeWhile(_ % 2 == 0))
    println(stream1.takeWhile(_ % 2 == 1))

    println(Stream(1, 2, 3, 4).headOption())
    println(Stream().headOption())

    println(stream1.map(i => {
      println("sum");
      i + 2
    }))

    println("----")

    println(Stream.constant("c").take(5).toList())
    println(Stream.from(10).take(5).toList())
    println(Stream.fib().take(7).toList())

    val fibs = Stream.unfold[Int, (Int, Int)]((0, 1)) {
        case (n1, n2) => Some((n1, (n2, n1+n2)))
    }
    println(fibs.take(7).toList())
  }
}

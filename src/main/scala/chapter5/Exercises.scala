package chapter5

import Stream.cons

object Exercises {
  def main(): Unit = {
    val s = Stream(1,2,3,4,5,6)

    println(s.takeWhile(_%3 == 1).toList())
    println(s.map(i => (i+10).toString() + "'").toList())
    println(s.take(3).toList())

    val s2 = Stream(10, 11, 12, 13)

    println(Stream.zipWith(s, s2)(_ + _).toList())
    println(Stream.zipAll(s,s2).toList())

    println("====================")

    val a = Stream(1,2,3,4,5,6)
    val b = Stream(1,2,3)
    println(a startsWith b)
    println(b startsWith a)
    println(b startsWith a.take(3))
    println(a startsWith Empty)

    println("====================")

    val c = cons(1, cons(2, Cons(() => 3, () => Cons(() => {println("4"); 4}, () => Empty))))
    println(c startsWith Stream(1,2))
    println(c startsWith Stream(1,2,3))

    println("====================")
    println(c.exists(_>2))
    println(a.tails.map(_.toList()).toList())

    println(c hasSubSequence Stream(1,2,23))
    
  }
}

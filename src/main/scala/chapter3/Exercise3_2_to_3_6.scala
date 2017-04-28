package chapter3

import List._

object Exercise3_2_to_3_6 {


  def main(): Unit = {
    val empty = Nil
    val oneElem = List(4)
    val twoElem = List("one", "two")
    val tenElem = List(0,1,2,3, 4,5,6,7,8,9)

    println(tail(empty))
    println(tail(oneElem))
    println(tail(tenElem))

    println(setHead("new", twoElem))
    println(setHead("new", empty))
    println(setHead(5, empty))

    println(drop(tenElem, 4))

    println(dropWhile[Int](tenElem, _ < 5))

    println(init(tenElem))
  }


}

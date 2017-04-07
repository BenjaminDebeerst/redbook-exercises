package chapter2

object Exercise2_2 {
  def main(): Unit = {
    println(isSorted[Int](Array(1), _ < _))
    println(isSorted[Int](Array(1,2,4), _ < _))
    println(isSorted[Int](Array(1,2,3,1,5,6), _ < _))
    println(isSorted[String](Array("abc","do some","order","so naturally", "which is lexically"), _ < _))
    println(isSorted[String](Array("order","strings by", "their length...."), _.length < _.length))
  }

  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    // this could be something like this:
//    as match {
//      case a1 :: a2 :: tail => (ordered(a1, a2) && isSorted(Array(a2, tail), ordered))
//      case _ => true
//    }
    if(as.length < 2) true
    else ordered(as(0), as(1)) && isSorted(as.slice(1, as.length), ordered)
  }

}

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
    as match {
      case Array(a1, a2, _*) => ordered(a1,a2) && isSorted(as.slice(1,as.length), ordered)
      case _ => true
    }
  }

}

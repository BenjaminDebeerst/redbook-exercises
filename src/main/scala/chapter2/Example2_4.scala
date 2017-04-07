package chapter2

object Example2_4 {
  def main(): Unit = {
    println(formatResult("absolute value", -42, abs))
    println(formatResult("square", 5, sqr))
    println(formatResult("square root", 30.0, sqrt))
  }

  def formatResult[T](arg: String, v: T, f: T => T) : String= {
    "The %s of %s is %s.".format(arg, v, f(v))
  }

  def abs(n: Int) = {
    if (n < 0) -n
    else n
  }

  def sqr(n: Int) = {
    n*n
  }

  def sqrt(d: Double) = {
    Math.sqrt(d)
  }

}
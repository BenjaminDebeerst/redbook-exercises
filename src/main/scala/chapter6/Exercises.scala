package chapter6


object Exercises {
  def main(): Unit = {

    printN[RNG, Int](
      3,
      SimpleRNG(42),
      rng => rng.nextInt
    )
    println("--")
    printN[RNG, Int](
      3,
      SimpleRNG(42),
      rng => nonNegativeInt(rng)
    )
    println("--")
    printN[RNG, Double](
      20,
      SimpleRNG(42),
      rng => double(rng)
    )

    println("--")
    printN[RNG, (Int, Double)](
      5,
      SimpleRNG(42),
      rng => intDouble(rng)
    )

    println("--")
    printN[RNG, (Double, Int)](
      20,
      SimpleRNG(42),
      rng => doubleInt(rng)
    )

    println("--")
    printN[RNG, (Double, Double, Double)](
      5,
      SimpleRNG(42),
      rng => double3(rng)
    )

  }

  def printN[A, B](n: Int, seed: A, next: A => (B, A), printer: B => Unit = println(_: B)): Unit = {
    n match {
      case 0 => Unit
      case _ => {
        val (a, b) = next(seed)
        printer(a)
        printN(n - 1, b, next, printer)
      }
    }
  }

  trait RNG {
    def nextInt: (Int, RNG)
  }

  case class SimpleRNG(seed: Long) extends RNG {
    override def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNF = SimpleRNG(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRNF)
    }
  }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (n, nextRng) = rng.nextInt
    n match {
      case _ if n < 0 => nonNegativeInt(nextRng)
      case _ => (n, nextRng)
    }
  }

  def double(rng: RNG): (Double, RNG) = {
    val (n, nextRng) = nonNegativeInt(rng)
    n match {
      case Int.MaxValue => double(nextRng) // avoid returning 1
      case _ => (n.toDouble / Int.MaxValue, nextRng)
    }
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, nextRng) = rng.nextInt
    val (d, _) = double(rng)
    ((i, d), nextRng)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (i, _) = rng.nextInt
    val (d, nextRng) = double(rng)
    ((d, i), nextRng)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, rng1) = double(rng)
    val (d2, rng2) = double(rng1)
    val (d3, rng3) = double(rng2)
    ((d1, d2, d3), rng3)
  }
}

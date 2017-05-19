package chapter6


object Exercises {
  def main(): Unit = {

    printN[RNG, Int](
      10,
      SimpleRNG(42),
      rng => nonNegativeEven(rng)
    )
    println("--")
    printN[RNG, Double](
      2,
      SimpleRNG(23),
      rng => doubleWithMap(rng)
    )
    printN[RNG, Double](
      2,
      SimpleRNG(23),
      rng => double(rng)
    )

    println("--")
    printN[RNG, (Int, Double)](
      2,
      SimpleRNG(23),
      rng => intDoubleWithMap(rng)
    )
    printN[RNG, (Int, Double)](
      2,
      SimpleRNG(23),
      rng => intDouble(rng)
    )

    println("---")

    printN[RNG, List[Int]](
      10,
      SimpleRNG(23),
      sequence[Int](List(_.nextInt, nonNegativeEven, nonNegativeInt))
    )

    println(ints(10)(SimpleRNG(23)))
    println(intsWithSequence(10)(SimpleRNG(23)))
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

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] = {
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }
  }

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    rng => {
      val (a, rng2) = ra(rng)
      val (b, rng3) = rb(rng2)
      (f(a,b), rng3)
    }
  }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
    fs.foldRight(unit(List[A]()))( (ra, rl) => map2(ra, rl)( _ :: _) )
  }

  def intsWithSequence(count: Int) = sequence(List.fill(count)(int))

  def ints(count: Int): Rand[List[Int]] = {
      count match {
        case 0 => unit(List[Int]())
        case _ => rng => {
          val (l, rng2) = ints(count-1)(rng)
          val (i, rng3) = rng2.nextInt
          (i :: l, rng3)
        }
      }
  }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] = map2(ra, rb)((_, _))

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

  def doubleWithMap: Rand[Double] = {
    map[Int, Double](nonNegativeInt)( _.toDouble / Int.MaxValue)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, nextRng) = rng.nextInt
    val (d, _) = double(rng)
    ((i, d), nextRng)
  }

  def intDoubleWithMap: Rand[(Int, Double)] = both(_.nextInt, double)

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

  def nonNegativeEven: Rand[Int] = {
      map[Int, Int](nonNegativeInt)(i => i-i %2)
  }
}

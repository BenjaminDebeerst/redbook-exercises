package chapter6 

object CandyMachine {
  def run(): Unit = {
    println("Candy!") 
    val m = Machine(true, 5, 10)
    val in = List(Turn, Coin, Turn, Turn)
    println(m)
    println(simulateMachine(in)(m)._2)
    println("=== Tests ===")
    val testMachine = Machine(true, 10, 10)
    println(simulateMachine(List(Coin, Turn, Coin, Turn))(testMachine)._2, "should be Machine(true, 8, 12")
    println(simulateMachine(List(Coin, Coin, Coin, Coin))(testMachine)._2, "should be Machine(false, 10, 11")
    println(simulateMachine(List(Turn, Turn, Turn, Turn))(testMachine)._2, "should be Machine(true, 10, 10")

    println(simulateMachine(List(Coin))(Machine(true, 0, 5))._2, "should be Machine(true, 0, 5")
    println(simulateMachine(List(Turn))(Machine(false, 0, 5))._2, "should be Machine(false, 0, 5")
    println(simulateMachine(List(Coin))(Machine(false, 0, 5))._2, "should be Machine(false, 0, 5")
    println(simulateMachine(List(Turn))(Machine(true, 5, 5))._2, "should be Machine(false, 5, 5")
  }

  def simulateMachine(inputs: List[Input]): State[Machine, Any] = State(
    m => (Unit, inputs.foldLeft(m)((m, i) => m.handle(i)))
  )

}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int) {
  def handle(input: Input): Machine = {
    (input, locked, candies, coins) match {
      case (_, _, 0, _) => this
      case (Coin, false, _, _) => this
      case (Turn, true, _, _) => this
      case (Coin, true, a, b) => Machine(false, a, b + 1)
      case (Turn, false, a, b) => Machine(true, a-1, b)
    }
  }
}



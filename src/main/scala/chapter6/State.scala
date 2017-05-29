package chapter6 

import Exercises._

object Exercise6_10 {
  def run(): Unit = {
    print[Any, Int](State.unit(3), Unit, 5)
    print[RNG, Int](State(_.nextInt))
    println("---")
    val rand10 = State[RNG, Int](_.nextInt).map(_%5 + 5)
    print(rand10)
    print(rand10.mapWith(rand10)((_,_)), n=3)
    print(rand10.mapWith(rand10)(_ + _), n=3)
    println("---")
    val int = State[RNG, Int](_.nextInt)
    lazy val nonNegativeInt: State[RNG, Int] = int.flatMap {
      a: Int =>
        if(a<0) nonNegativeInt
        else State.unit(a)
      }
    print(nonNegativeInt, n=3)
    val prependPlus = int.map("+" + _)
    print(prependPlus, n=3)
    val double01: State[RNG, Double] = nonNegativeInt.map(_ / (Int.MaxValue.toDouble + 1))
    print(double01)
    println("---")
    print(State.sequence[RNG, Int](List(int, rand10, nonNegativeInt)))
    println("---")
    print(State.sequence(List(int, int, int)), n=4)
  }

  def print[S, A](action: State[S, A], seed: S = SimpleRNG(42), n: Int = 5): Unit = {
    if(n == 0)
      Unit
    else {
      val (a, nextS) = action(seed)
      println(a)
      print(action, nextS, n-1)
    }
  }
}

case class State[S, +A](run: S => (A,S)) {
  def apply(s: S) = run(s)
 
  def map[B](f: A => B): State[S, B] =
    flatMap(a => State((f(a), _)))

  def mapWith[B, C](sb: State[S, B])(f: (A, B) => C): State[S,C] =
    flatMap(a => sb.map(f(a, _)))

  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State[S, B](s => {
      val (a, s2) = run(s)
      f(a)(s2)
    })
 
  def modify(f: S => S): State[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()

  def get: State[S, S] = State(s => (s, s))
  
  def set(s: S): State[S, Unit] = State(_ => ((), s))
}   
 
object State {
  def unit[S, A](a: A): State[S, A] = State((a, _))

  def sequence[S, A](actions: List[State[S, A]]): State[S, List[A]] =
    actions.foldRight(State.unit[S, List[A]](List()))(
      (sa, sl) => sa.mapWith(sl)(_ :: _)
    )
} 


package chapter7

import java.time.{Duration, LocalTime}
import java.util.concurrent._


object Parallelism {
  import Par.Par

  def run(): Unit = {
    val executor = Executors.newWorkStealingPool(200)
    val numbers = IndexedSeq(1,2,3,4,5,6)

    def f(a: Int): Int = { println("computing"); a + 1}
    val lazyF = Par.asyncF(f)
    val lazyResult = lazyF(42)
    println(lazyResult)
    println("-----")
    println(Par.run(executor)(lazyResult).get)

    measureTime(
      () => {
        val result: Int = Par.run(executor)(sum(numbers, timeConsumingSum)).get
        println("Parallel computation: " + result, result.getClass)
      }
    )

    println("======")
    measureTime(
      () => {
        println((Par.parMap(List.fill(200)(1))(i => timeConsumingSum(i, 1)))(executor).get)
      }
    )


    executor.shutdownNow()
  }

  def timeConsumingSum(a: Int, b: Int): Int = {
    Thread.sleep(1000)
    a + b
  }

  def sum(ints: IndexedSeq[Int], addition: (Int, Int) => Int = _ + _): Par[Int] = {
    if (ints.size < 2)
      Par(ints.headOption.getOrElse(0))
    else {
      val (l, r) = ints.splitAt(ints.size / 2)
      Par.map2(Par.fork(sum(l)), Par.fork(sum(r)))(addition)
    }
  }

  def measureTime(task: () => Unit) = {
    val before = LocalTime.now()
    task()
    val after = LocalTime.now()
    println("Took " + Duration.between(before, after).getSeconds + " seconds\n")
  }

}

object Par {

  type Par[A] = ExecutorService => Future[A]

  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)
  def apply[A](a: => A): Par[A] = unit(a)

  def map2[A, B, C](a: => Par[A], b: => Par[B])(f: (A, B) => C): Par[C] =
    (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)
      UnitFuture(f(af.get, bf.get))
    }

  def fork[A](a: => Par[A]): Par[A] =
    es => es.submit(new Callable[A] {
      override def call = a(es).get
    })

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  def asyncF[A,B](f: A => B): A => Par[B] = a => lazyUnit(f(a))

  def sortPar(parList: Par[List[Int]]): Par[List[Int]] = map(parList)(_.sorted)

  def map[A,B](pa: Par[A])(f: A => B): Par[B] = map2(pa, unit(()))((a, _) => f(a))

  def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = {
    val fbs: List[Par[B]] = ps.map(asyncF(f))
    sequence(fbs)
  }

  def sequence[A](ps: List[Par[A]]): Par[List[A]] =
    ps.foldRight(Par.unit(List[A]()))((l, pa) => map2(l, pa)((a, l) => a :: l))

  private case class UnitFuture[A](get: A) extends Future[A] {
    override def isDone = {println("is done?") ; true }
    override def get(timeout: Long, unit: TimeUnit) = {println(s"get $timeout $unit"); get}
    override def isCancelled = {println("cancelled?"); false}
    override def cancel(evenIfRunning: Boolean) = {println(s"cancel $evenIfRunning"); false}
  }

}






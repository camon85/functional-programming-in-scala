package chap6

import scala.annotation.tailrec

case class SimpleRNG(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}

object SimpleRNG {
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (num, newRng) = rng.nextInt
    if (num < 0) {
      (-(num + 1), newRng)
    } else {
      (num, newRng)
    }
  }

  def double(rng: RNG): (Double, RNG) = {
    val (num, newRng) = nonNegativeInt(rng)
    (num / (Int.MaxValue.toDouble + 1), newRng)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, rng2) = rng.nextInt
    val (d, rng3) = double(rng2)
    ((i, d), rng3)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((i, d), rng2) = intDouble(rng)
    ((d, i), rng2)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, rng2) = double(rng)
    val (d2, rng3) = double(rng2)
    val (d3, rng4) = double(rng3)
    ((d1, d2, d3), rng4)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    if (count < 1) {
      (List(), rng)
    } else {
      val (num, rng2) = rng.nextInt
      val (num2, rng3) = ints(count - 1)(rng2)
      (num :: num2, rng3)
    }
  }

  def intsTailRecursive(count: Int)(rng: RNG): (List[Int], RNG) = {
    @tailrec
    def loop(count: Int, acc: List[Int], rng: RNG): (List[Int], RNG) = {
      if (count < 1) {
        (acc, rng)
      } else {
        val (num, rng2) = rng.nextInt
        loop(count - 1, num :: acc, rng2)
      }
    }
    loop(count, Nil, rng)
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeEven: Rand[Int] =
    map(nonNegativeInt)(i => i - i % 2)

  def doubleByMap: Rand[Double] = {
    map(nonNegativeInt)(d => d / (Int.MaxValue.toDouble + 1))
  }

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, rng2) = ra(rng)
      val (b, rng3) = rb(rng2)
      (f(a, b), rng3)
    }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
    map2(ra, rb)((_, _))

  def randIntDouble: Rand[(Int, Double)] = both(int, double)

  def randDoubleInt: Rand[(Double,Int)] = both(double, int)

  def main(args: Array[String]): Unit = {
    println("== 연습문제 6.1 == nonNegativeInt ")
    println(nonNegativeInt(SimpleRNG(1)))

    println("== 연습문제 6.2 == double")
    println(double(SimpleRNG(1)))

    println("== 연습문제 6.3 == intDouble, doubleInt, double3 ")
    println(intDouble(SimpleRNG(1)))
    println(doubleInt(SimpleRNG(1)))
    println(double3(SimpleRNG(1)))

    println("== 연습문제 6.4 ==  ints")
    println(ints(-1)(SimpleRNG(1))._1)
    println(ints(0)(SimpleRNG(1))._1)
    println(ints(1)(SimpleRNG(1))._1)
    println(ints(10)(SimpleRNG(1))._1)
    println(intsTailRecursive(10)(SimpleRNG(1))._1)

    println("== 연습문제 6.5 == map을 이용해서 double 구현 ")
    println(doubleByMap(SimpleRNG(1))_1)

    println("== 연습문제 6.6 == map2 구현 ")
//    println(map2(nonNegativeEven, double)(_ + _))

    println("== 연습문제 6.7 == sequence 구현 ")
    println()

//    println("== 연습문제 6.8 == flatMpa을 구현하고 그것을 이용해서 nonNegativeLessThan 구현")
//    println()
//
//    println("== 연습문제 6.9 == map과 map2를 flatMap을 이용해서 다시 구현하라")
//    println()
//
//    println("== 연습문제 6.10 == unit, map, map2, flatMap, sequence를 일반화하라")
//    println()
//
//    println("== 연습문제 6.11 == state automata 구현")
//    println()


  }

}
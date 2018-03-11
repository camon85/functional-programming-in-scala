package chap02

// 연습문제 2.1
object Fibonacci {

  /*
  n 번째 피보나치 수는 항상 이전 두 수의 합이다.
  0, 1, 1, 2, 3, 5
  */
  def fib(n: Int): Int = {
    if (n <= 1) n
    else fib(n - 2) + fib(n - 1)
  }

  def fib2(n: Int): Int = {
    def loop(a: Int, b: Int, n:Int): Int = {
      println(s"a: $a, b: $b, n: $n")
      if (n < 1) a
      else loop(b, a + b, n - 1)
    }

    loop(0, 1, n)
  }

  /*
  0: 0
  1: 1
  2: 0 + 1
  3: 1 + 1 = 2
  4: 2 + 1 = 3
  5: 3 + 2 = 5

   */


  def main(args: Array[String]): Unit = {
//    println("Fibonacci Numbers: " + fib(5))
    println("Fibonacci2 Numbers (0): " + fib2(0))
    println("Fibonacci2 Numbers (1): " + fib2(1))
    println("Fibonacci2 Numbers (2): " + fib2(2))
    println("Fibonacci2 Numbers (3): " + fib2(3))
    println("Fibonacci2 Numbers (4): " + fib2(4))
    println("Fibonacci2 Numbers (5): " + fib2(5))
  }

}

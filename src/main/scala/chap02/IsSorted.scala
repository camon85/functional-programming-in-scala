package chap02

// 연습문제 2.2
object IsSorted {

  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    def loop(n: Int): Boolean = {
      if (as.length == 1) true
      else if (n == as.length - 1) true
      else if (!ordered(as(n), as(n + 1))) false
      else loop(n + 1)
    }

    loop(0)
  }


  /*
  size: 5
  n: 0
  n: 1
  n: 2
  n: 3
  n: 4
  n: 5
   */
  def numberAscOrder(prev: Int, next: Int): Boolean = {
    prev <= next
  }

  def stringLengthAscOrder(prev: String, next: String): Boolean = {
    prev.length <= next.length
  }

  def main(args: Array[String]): Unit = {
    println(isSorted(Array(1,2,3,4,5), numberAscOrder))
    println(isSorted(Array(3,2,1,4,5), numberAscOrder))
    println(isSorted(Array("a", "abc", "abcde"), stringLengthAscOrder))
    println(isSorted(Array("abc", "abc", "a"), stringLengthAscOrder))
  }

}

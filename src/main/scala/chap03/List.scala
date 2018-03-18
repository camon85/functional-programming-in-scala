package chap03

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def tail[A](as: List[A]): List[A] =
    as match {
      case Cons(_, t) => t
    }

  def setHead[A](as: List[A], newValue: A): List[A] = {
    as match {
      case Cons(_, t) => Cons(newValue, t)
    }
  }

  def drop[A](l: List[A], n: Int): List[A] = {
    def loop(list: List[A], count: Int): List[A] =
      if (count < 1) list
      else loop(tail(list), count - 1)

    loop(l, n)
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
    def loop(list: List[A]): List[A] = {
      val head = list match {
        case Cons(h, _) => h
      }

      if (!f(head)) list
      else loop(tail(list))
    }
    loop(l)
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }

  def init[A](l: List[A]): List[A] = {
    l match {
      case Cons(_, Nil) => Nil
      case Cons(h, _) => append(List(h), init(tail(l)))
    }
  }

  def main(args: Array[String]): Unit = {
    println(List("a","b") == Cons("a", Cons("b", Nil)))

    val ex1: List[Double] = Nil
    val ex2: List[Int] = Cons(1, Nil)
    val ex3: List[String] = Cons("a", Cons("b", Nil))

    println(ex1)
    println(ex2)
    println(ex3)

    println("== 연습문제 3.1 표현식의 결과 ==")
    val xValue = List(1,2,3,4,5) match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4,_)))) => x + y
      case Cons(h, t) => h + sum(t)
      case _ => 101
    }
    println(xValue) // 3

    println("== 연습문제 3.2 tail ==")
    println(tail(List("A","B","C"))) // Cons(B,Cons(C,Nil))
    println(tail(List("1","2","3"))) // Cons(2,Cons(3,Nil))

    println("== 연습문제 3.3 setHead ==")
    println(setHead(List("A","B","C"), "X")) // Cons(X,Cons(B,Cons(C,Nil)))
    println(setHead(List(1,2,3), 999)) // Cons(999,Cons(2,Cons(3,Nil)))

    println("== 연습문제 3.4 drop ==")
    println(drop(List("A","B","C"), 1)) // Cons(B,Cons(C,Nil))
    println(drop(List("A","B","C"), 3)) // Nil

    println("== 연습문제 3.5 dropWhile ==")
    // 조건에 해당하는 요소를 drop 시킨다. (첫 실패 지점까지 실행)
    println(dropWhile(List(1,2,3,4,5), (x: Int) => x < 3)) // Cons(3,Cons(4,Cons(5,Nil)))

    // 처음부터 조건 실패 했으니 그대로 돌려준다.
    println(dropWhile(List(1,2,3,4,5), (x: Int) => x > 3)) // Cons(1,Cons(2,Cons(3,Cons(4,Cons(5,Nil)))))

    println("== 연습문제 3.6 init ==")
    println(init(List(1,2,3,4,5))) // Cons(1,Cons(2,Cons(3,Cons(4,Nil))))


    println("== 연습문제 3.7 ==")
    println("== 연습문제 3.8 ==")
    println("== 연습문제 3.9 ==")
//    println("== 연습문제 3.10 ==")
//    println("== 연습문제 3.11 ==")
//    println("== 연습문제 3.12 ==")
//    println("== 연습문제 3.13 ==")
//    println("== 연습문제 3.14 ==")
//    println("== 연습문제 3.15 ==")
//    println("== 연습문제 3.16 ==")
//    println("== 연습문제 3.17 ==")
//    println("== 연습문제 3.18 ==")
//    println("== 연습문제 3.19 ==")
//    println("== 연습문제 3.20 ==")
//    println("== 연습문제 3.21 ==")
//    println("== 연습문제 3.22 ==")
//    println("== 연습문제 3.23 ==")
//    println("== 연습문제 3.24 ==")
//    println("== 연습문제 3.25 ==")
//    println("== 연습문제 3.26 ==")
//    println("== 연습문제 3.27 ==")
//    println("== 연습문제 3.28 ==")
//    println("== 연습문제 3.29 ==")



  }

}

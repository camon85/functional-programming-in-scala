package chap03

import scala.annotation.tailrec

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int =
    ints match {
      case Nil => 0
      case Cons(x, xs) => x + sum(xs)
    }

  def product(ds: List[Double]): Double =
    ds match {
      case Nil => 1.0
      case Cons(0.0, _) => 0.0
      case Cons(x, xs) => x * product(xs)
    }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def tail[A](as: List[A]): List[A] =
    as match {
      case Nil => Nil
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

  // 최초 생각한 loop버전
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

  // case문에서 if를 사용한 버전
  def dropWhile2[A](l: List[A], f: A => Boolean): List[A] = {
    l match {
      case Cons(h, _) if !f(h) => l
      case _ => dropWhile2(tail(l), f)
    }
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

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
    as match {
      case Nil => z
      case Cons(h, t) => f(h, foldRight(t, z)(f))
    }
  }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)(_ + _)
//    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _)
//    foldRight(ns, 1.0)((x, y) => x * y)

  def length[A](as: List[A]): Int = {
    foldRight(as, 0)((_, z) => 1 + z)
  }

  @tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = {
    as match {
      case Nil => z
      case Cons(h, t) => foldLeft(t, f(z, h))(f)
    }
  }

  def sumByFoldLeft(ints: List[Int]): Int =
    foldLeft(ints, 0)(_ + _)

  def productByFoldLeft(ns: List[Double]): Double =
    foldLeft(ns, 1.0)(_ * _)

  def lengthByFoldLeft[A](as: List[A]): Int =
    foldLeft(as, 0)((z, _) => z + 1)

  def reverse[A](as: List[A]) =
    foldLeft(as, Nil: List[A])((x, y) => Cons(y, x))

  def appendByFolRight[A](a1: List[A], a2: List[A]): List[A] =
    foldRight(a1, a2)((x, y) => Cons(x, y))

  def concat[A](list: List[List[A]]) =
    foldRight(list, Nil: List[A])(append)

  def addOne(ns: List[Int]) =
    foldRight(ns, Nil: List[Int])((h, t) => Cons(h+1, t))

  def doubleListToStringList(ds: List[Double]) =
    foldRight(ds, Nil:List[String])((h,t) => Cons(h.toString, t))

  def map[A,B](as: List[A])(f: A => B): List[B] =
    foldRight(as, Nil: List[B])((h, t) => Cons(f(h), t))

  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    foldRight(as, Nil: List[A])((h, t) => if (f(h)) Cons(h, t) else t)

  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] =
    concat(map(as)(f))

  def filterByFlatMap[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as)((x) => if (f(x)) Cons(x, Nil) else Nil)

  def zip(a1: List[Int], a2: List[Int]): List[Int] =
    (a1, a2) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(h1, t1), (Cons(h2, t2))) => Cons(h1 + h2, zip(t1, t2))
    }

  def zipWith[A,B,C](as: List[A], bs: List[B])(f: (A, B) => C): List[C] =
    (as, bs) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(h1, t1), (Cons(h2, t2))) => Cons(f(h1, h2), zipWith(t1, t2)(f))
    }

//  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean =
//    sup match {
//      case Nil => sub == Nil
//      case Cons(h, t) => h == sub의 head
//    }

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
    println(dropWhile2(List(1,2,3,4,5), (x: Int) => x < 3)) // Cons(3,Cons(4,Cons(5,Nil)))

    // 처음부터 조건 실패 했으니 그대로 돌려준다.
    println(dropWhile(List(1,2,3,4,5), (x: Int) => x > 3)) // Cons(1,Cons(2,Cons(3,Cons(4,Cons(5,Nil)))))
    println(dropWhile2(List(1,2,3,4,5), (x: Int) => x > 3)) // Cons(1,Cons(2,Cons(3,Cons(4,Cons(5,Nil)))))

    println("== 연습문제 3.6 init ==")
    println(init(List(1,2,3,4,5))) // Cons(1,Cons(2,Cons(3,Cons(4,Nil))))

    println("== 연습문제 3.7 ==")
    // foldRight가 list를 모두 펼친 다음에 오른쪽부터 평가가 시작 된다.
    // 중간에 재귀를 멈추지 않는다.

    println("== 연습문제 3.8 == foldRight와 List자료 생성자들 사이의 관계")
    println(foldRight(List(1,2,3), Nil:List[Int])(Cons(_, _))) // Cons(1,Cons(2,Cons(3,Nil)))
    // foldRight는 Nil을 받으면 리스트의 다음 요소가 존재하더라도 않고 Nil을 돌려줘 버린다.
    // 생서자를 넣으면 복사된다.

    println("== 연습문제 3.9 length ==")
    println(length(List(1,2,3,4,5,6,7,8)))

    println("== 연습문제 3.10 foldLeft ==")
    // 계산을 먼저 하고 loop를 돌려야 꼬리재귀 된다..
    // foldRight는 꼬리재귀로 만들 수 없는건가?
    println(foldLeft(List(1,2,3), 0)(_ + _)) // 6

    println("== 연습문제 3.11 sum, product, length by foldLeft ==")
    println(sumByFoldLeft(List(1, 2, 3, 10))) // 16
    println(productByFoldLeft(List(1.0, 2.0, 10.0))) // 20.0
    println(length(List(1, 2, 3, 4, 5, 6, 7, 8))) // 8

    println("== 연습문제 3.12 목록의 역을 돌려주는 함수 ==")
    println(reverse(List(1, 2, 3))) // Cons(3,Cons(2,Cons(1,Nil)))

    println("== 연습문제 3.13 == foldLeft를 foldRight를 이용해서 구현 ")
    println("TODO")
    // revers 한 다음에 foldLeft

    println("== 연습문제 3.14 == append를 foldLeft나 foldRight를 이용해서 구현")
    println(appendByFolRight(List(1,2,3), List(4,5,6))) // Cons(1,Cons(2,Cons(3,Cons(4,Cons(5,Cons(6,Nil))))))

    println("== 연습문제 3.15 == 목록들의 목록을 하나로 연결 ")
    println(concat(List(List(1,2), List(3,4), List(5,6)))) // Cons(1,Cons(2,Cons(3,Cons(4,Cons(5,Cons(6,Nil))))))
    // foldLeft는 선형시간에 비례 하지않는다. n제곱 필요

    println("== 연습문제 3.16 == 목록에 1을 더해서 반환")
    println(addOne(List(1,2,3))) // Cons(2,Cons(3,Cons(4,Nil)))

    println("== 연습문제 3.17 == List[Double]을 List[String]으로 변환")
    val stringList: List[String] = doubleListToStringList(List(1.0, 2.0, 3.0))
    println(stringList)

    println("== 연습문제 3.18 == map 구현")
    println(map(List(1,2,3))((x: Int) => x.toString)) // Cons(1,Cons(2,Cons(3,Nil)))
    println(map(List("abcde", "ab", "12345"))((x: String) => x.length)) // Cons(5,Cons(2,Cons(5,Nil)))

    println("== 연습문제 3.19 == filter 구현")
    println(filter(List(1,2,5,6))(_ < 3)) // Cons(1,Cons(2,Nil))

    println("== 연습문제 3.20 == flatMap 구현")
    println(flatMap(List(1,2,3))(i => List(i,i))) // Cons(1,Cons(1,Cons(2,Cons(2,Cons(3,Cons(3,Nil))))))

    println("== 연습문제 3.21 == flatMap을 이용해서 filer구현")
    println(filterByFlatMap(List(1,2,5,6))(_ < 3)) // Cons(1,Cons(2,Nil))

    println("== *연습문제 3.22 == 목록 두개를 받아 대응되는 요소를 더한 값으로 새로운 목록 구축")
    println(zip(List(1,2,3), List(4,5,6))) // Cons(5,Cons(7,Cons(9,Nil)))

    println("== 연습문제 3.23 ==")
    println(zipWith(List(1,2,3), List(4,5,6))(_ + _)) // Cons(5,Cons(7,Cons(9,Nil)))
    println(zipWith(List("my", "scala", "study"), List(8, 5, 5))((a, b) => a.length + b)) // Cons(10,Cons(10,Cons(10,Nil)))

    println("== 연습문제 3.24 == hasSubsequence")
    println()
    // startWIth를 먼저 구현하자.
  }

}

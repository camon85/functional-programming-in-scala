package chap05

sealed trait Stream[+A] {
  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, t) => Some(h())
  }

  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h() :: t().toList
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h, _) if n == 1 =>  Stream.cons(h(), Empty)
    case Cons(h, t) if n > 1 =>  Stream.cons(h(), t().take(n - 1))
    case _ => Empty
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n == 1 => t()
    case Cons(_, t) if n > 1 => t().drop(n - 1)
    case _ => this
  }


  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) =>  Stream.cons(h(), t().takeWhile(p))
    case _ => Empty
  }

  def exists(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => p(h()) || t().exists(p)
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  def existsByFoldRight(p: A => Boolean): Boolean =
    foldRight(false)((h, t) => p(h) || t)

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((h, t) => p(h) && t)

  def takeWhileByFoldRight(p: A => Boolean): Stream[A] =
    foldRight(Empty: Stream[A])((h, t) => if (p(h)) Stream.cons(h, t) else Empty)

  def headOptionByFoldRight: Option[A] =
    foldRight(None: Option[A])((h, _) => Some(h))

  def map[B](f: A => B): Stream[B] =
    foldRight(Empty: Stream[B])((h, t) => Stream.cons(f(h), t))

  def filter(f: A => Boolean): Stream[A] =
    foldRight(Empty: Stream[A])((h, t) => if (f(h)) Stream.cons(h, t) else t)

  def append[B>:A](s: => Stream[B]): Stream[B] =
    foldRight(s)((h, t) => Stream.cons(h, t))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(Empty: Stream[B])((h, t) => f(h).append(t))

}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(()=> head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  def main(args: Array[String]): Unit = {
    println("== 연습문제 5.1 == toList 구현 ")
    println(Stream(1,2,3,4,5).toList) // List(1, 2, 3, 4, 5)

    println("== 연습문제 5.2 take, drop == ")
    println(Stream(1,2,3,4,5).take(3).toList) // List(1, 2, 3)
    println(Stream(1,2,3,4,5).drop(3).toList) // List(4, 5)

    println("== 연습문제 5.3 == takeWhile")
    println(Stream(1,2,3,4,5).takeWhile(x => x < 3).toList) // List(1, 2)

    println("== 연습문제 5.4 == forAll ")
    println(Stream(1,2,3,4,5).forAll(x => x < 10)) // true
    println(Stream(1,2,3,4,5).forAll(x => x < 3)) // false

    println("== 연습문제 5.5 == takeWhile ")
    println(Stream(1,2,3,4,5).takeWhileByFoldRight(x => x < 3).toList) // List(1, 2)

    println("== 연습문제 5.6 == headOption ")
    println(Stream(1,2,3,4,5).headOptionByFoldRight) // Some(1)

    println("== 연습문제 5.7 == map, filter, append, flatMap")
    println(Stream(1,2,3,4,5).map(_ * 10).toList) // List(10, 20, 30, 40, 50)
    println(Stream(1,2,3,4,5).filter(_ % 2 == 0).toList) // List(2, 4)
    println(Stream(1,2,3,4,5).append(Stream(6,7,8,9,10)).toList) // List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
    println(Stream(1,2,3,4,5).flatMap(x => Stream(x * 2)).toList) // List(2, 4, 6, 8, 10)

//    println("== 연습문제 5.8 == ")
//    println()
//    println("== 연습문제 5.9 == ")
//    println()
//    println("== 연습문제 5.10 == ")
//    println()
//    println("== 연습문제 5.11 == ")
//    println()
//    println("== 연습문제 5.12 == ")
//    println()
//    println("== 연습문제 5.13 == ")
//    println()
//    println("== 연습문제 5.14 == ")
//    println()
//    println("== 연습문제 5.15 == ")
//    println()
//    println("== 연습문제 5.16 == ")
//    println()
//    println("== 연습문제 5.17 == ")
//    println()
//    println("== 연습문제 5.18 == ")
//    println()
//    println("== 연습문제 5.19 == ")
//    println()
  }
}

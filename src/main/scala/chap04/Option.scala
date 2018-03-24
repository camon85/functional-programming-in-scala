package chap04

sealed trait Option[+A] {

  def map[B](f: A => B): Option[B] = this match {
      case None => None
      case Some(a) => Some(f(a))
    }

  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case None => None
    case Some(a) => f(a)
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(a) => a
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
    case None => ob
    case _ => this
  }

  def filter(f: A => Boolean): Option[A] = this match {
    case Some(a) if f(a) => this
    case _ => None
  }

  // TODO flatMap, orElse, filter를 패턴매칭 없이 구현

}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object IntOption {

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)


  def main(args: Array[String]): Unit = {
    println("== 연습문제 4.1 == Option에 대한 함수들 구현")
    // getOrElse는 B를 준다.
    // orElse는 Option[B]를 준다.
    println(None.map((x: Int) => x + 5))
    println(Some(1).map(x => x + 5))

    println(Some(1).flatMap(x => Some(x + 5)))

    println(Some(None).getOrElse(Some(9999)))
    println(Some(500).getOrElse(Some(9999)))

    println(Some(10).orElse(Some(-1)))
    println(None.orElse(Some(-1)))

    println(Some(5).filter(_ < 3))
    println(Some(5).filter(_ > 3))



    println("== 연습문제 4.2 == ")


//    println("== 연습문제 4.3 == ")
//
//
//    println("== 연습문제 4.4 == ")
//
//
//    println("== 연습문제 4.5 == ")
//
//
//    println("== 연습문제 4.6 == ")
//
//
//    println("== 연습문제 4.7 == ")
//
//
//    println("== 연습문제 4.8 == ")

  }

}

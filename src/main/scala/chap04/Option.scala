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
  def flatMap2[B](f: A => Option[B]): Option[B] = ???

  def orElse2[B >: A](ob: => Option[B]): Option[B] = ???

  def filter2(f: A => Boolean): Option[A] = ???

}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  // 일단 xs의 평균 m이 필요하다.
  // 각 요소 x.  (math.pow(x - m, 2)) 이것의 평균
  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))

  // Option을 벗겨야 한다; flatMap? getOrElse?
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = (a, b) match {
    case (Some(x), Some(y)) => Some(f(x, y))
    case _ => None
  }
  def main(args: Array[String]): Unit = {
    println("== 연습문제 4.1 == Option에 대한 함수들 구현")
    // getOrElse는 B를 준다.
    // orElse는 Option[B]를 준다.
    println(None.map((x: Int) => x + 5)) // None
    println(Some(1).map(x => x + 5)) // Some(6)

    println(Some(1).flatMap(x => Some(x + 5))) // Some(6)

    println(Some(None).getOrElse(Some(9999))) // None
    println(Some(500).getOrElse(Some(9999))) // 500

    println(Some(10).orElse(Some(-1))) // Some(10)
    println(None.orElse(Some(-1))) // Some(-1)

    println(Some(5).filter(_ < 3)) // None
    println(Some(5).filter(_ > 3)) // Some(5)


    println("== 연습문제 4.2 == 분산(variance)을 flatMap을 이용해서 구현")
    // 분산은 순차열의 각 요소 x에 대한 math.pow(x - m, 2) 들의 평균.
    // https://ko.wikipedia.org/wiki/%EB%B6%84%EC%82%B0
    // 분산: 확률변수가 기댓값으로부터 얼마나 떨어진 곳에 분포하는지를 가늠하는 숫자
    // math.pow(밑, 지수) -> math.pow(2, 4) = 16.0
    println(variance(Seq(30.0, 40.0, 50.0))) // Some(66.66666666666667)
    println(variance(Nil)) // None

    println("== 연습문제 4.3 == binary function을 이용해서 결합하는 일반적 함수 map2를 작성")
    println(map2(Some(1), Some(2))(_ + _))

    println("== 연습문제 4.4 == ")


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

package chap04


sealed trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B] = this match {
    case Left(e) => Left(e)
    case Right(a) => Right(f(a))
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Left(e) => Left(e)
    case Right(a) => f(a)
  }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Left(_) => b
    case Right(a) => Right(a)
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = (this, b) match {
    case (Right(aa), Right(bb)) => Right(f(aa, bb))
    case (Left(e), _) => Left(e)
    case (_, Left(e)) => Left(e)
  }

  def map22[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C) =
    this.flatMap(aa => b.map(bb => f(aa, bb)))

  def map222[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C) =
    for {
      aa <- this
      bb <- b
    } yield f(aa, bb)

}
case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

object Either {

  def mean(xs: IndexedSeq[Double]): Either[String, Double] =
    if (xs.isEmpty)
      Left("mean of empty list!")
    else
      Right(xs.sum/ xs.length)

  def safeDiv(x: Int, y: Int): Either[Exception, Int] =
    try Right(x / y)
    catch {
      case e: Exception => Left(e)
    }

  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch {
      case e: Exception => Left(e)
    }

  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = traverse(es)(x => x)

  def sequence2[E, A](es: List[Either[E, A]]): Either[E, List[A]] =
    es.foldRight[Either[E, List[A]]](Right(Nil))((ee, el) => ee.map2(el)(_ :: _))

  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = as match {
    case Nil => Right(Nil)
    case h :: t => f(h).flatMap(hh => traverse(t)(f).map(hh :: _))
  }

  def traverse2[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    as.foldRight(Right(Nil):Either[E, List[B]])((ee, el) => f(ee).map2(el)(_ :: _))

  def main(args: Array[String]): Unit = {
    println("== 연습문제 4.6 == map, flatMap, orElse, map2 구현")
    println(Left("error").map((x: Int) => x + 5)) // Left(error)
    println(Right(1).map(x => x + 5)) // Right(6)

    println(Right(1).flatMap(x => Right(x + 5))) // Right(6)

    println(Right(10).orElse(Right(-1))) // Right(10)
    println(Left("error").orElse(Right(-1))) // Right(-1)

    println(Right(10).map2(Right(2))(_ + _)) // Right(12)
    println(Left(new Exception("test error")).map2(Right(2))((_: Int) + (_: Int))) // Left(java.lang.Exception: test error)

    println("== 연습문제 4.7 == sequence와 traverse 작성")
    println(sequence(List(Right(1), Right(2), Right(3)))) // Right(List(1, 2, 3))
    println(sequence(List(Right(1), Right(Nil), Right(2)))) // Right(List(1, List(), 2))
    println(sequence(List(Left("error")))) // Left(error)

    println(traverse(List(1, 2, 3))(x => Right(x.toString))) // Right(List(1, 2, 3))
    println(traverse(List(1, Nil, 3))(_ => Left("error"))) // Left(error)

    println("== 연습문제 4.8 == ")

  }

}

package chap06

case class State[S, +A](run: S => (A, S)) {

//  def mapByFlatMap[A, B](s: Rand[A])(f: A => B): Rand[B] =
//    flatMap(s)(a => unit(f(a)))

  def map[B](f: A => B): State[S, B] =
      flatMap(a => State.unit(f(a)))

//  def map2ByFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
//    flatMap(ra)(a => map(rb)(b => f(a, b)))

  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => sb.map(b => f(a, b)))

  //  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
  //    rng => {
  //      val (a, rng1) = f(rng)
  //      g(a)(rng1)
  //    }
  //
  // State를 run 하면 S => (A,S)가 나온다.
  // f를 실행하면 A => State[S, B]가 나온다.
  // f는 A => State[S, B].
  // f(a)를 run 하면 (A,S)가 나온다.
  // State로 감싸준다.
  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State(s => {
      val (a, s1) = run(s)
      f(a).run(s)
    })
}

object State {

  //  type Rand[+A] = RNG => (A, RNG)
  type Rand[A] = State[RNG, A]

  //  def unit[A](a: A): Rand[A] = rng => (a, rng)
  def unit[S, A](a: A): State[S, A] = State(s => (a, s))

  //  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
  //   fs.foldRight(unit(List[A]()))((a, b) => map2(a, b)(_ :: _))
  def sequence[S, A](sas: List[State[S, A]]): State[S, List[A]] =
    sas.foldRight(unit[S, List[A]](List()))((f, acc) => f.map2(acc)(_ :: _))

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  def main(args: Array[String]): Unit = {
    //
    //    println("== 연습문제 6.10 == unit, map, map2, flatMap, sequence를 일반화하라")
    //    println()
    //
    //    println("== 연습문제 6.11 == state automata 구현")
    //    println()
  }
}

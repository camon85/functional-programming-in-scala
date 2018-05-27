package chap08

import chap06.{RNG, SimpleRNG, State}
import chap08.Gen.Prop.{FailedCase, SuccessCount}

object Gen {

  object Prop {
    type FailedCase = String
    type SuccessCount = Int
  }

  trait Prop {
    def check: Either[(FailedCase, SuccessCount), SuccessCount]
  }

  case class Gen[A](sample: State[RNG, A])

  /*
   랜덤값 x 도출
   x % (stopExclusive - start) 하면 0부터 (stopExclusive - start -1)까지의 값이 나온다.
   거기에 + start 해준다.

   start: 5, stopExclusive: 10, x: 1000이라고 가정
   1000 % (10 - 5) 하면 0부터 4까지의 값이 나온다.
   우리는 5~9까지의 값이 필요하니까 start를 한번 더해주면 된다.
    */
  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(State(SimpleRNG.nonNegativeInt).map(n => n % (stopExclusive - start) + start))

  // 입력한 값을 Gen으로 감싸기
  def unit[A](a: A): Gen[A] = Gen(State.unit(a))

  // true나 false 생성
  val boolean: Gen[Boolean] = Gen(State(SimpleRNG.map(SimpleRNG.nonNegativeLessThan(2))(_ == 1)))

  /*
  // List.fill(n)(g) 한다음에 List[Gen]을 Gen[List]로 바꿔주면 될 것 같은데 그게 없다..

  List.fill(n)(g.sample) => List[State]가 생성된다.
  def sequence[S, A](sas: List[State[S, A]]): State[S, List[A]] 활용해서
  List[State]를 State[List]로 바꿔준다.

  Gen에 state를 넘겨주면 Gen[List] 생성 된다.
   */
  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
    Gen(State.sequence(List.fill(n)(g.sample)))

  def main(args: Array[String]): Unit = {
    /*
    forAll: 속성을 생성
    &&: 속성을 합성
    check: 속성을 점검
     */
    println("== 연습문제 8.1 == sum: List[Int] => Int 함수의 구현을 명시하는 속성들을 고안하라")
    println("뒤집어서 합산해도 값이 같아야 한다. => sum(nums) == sum(nums.reverse)")
    println("모든 요소의 값이 같다.(1,1,1,1,1): => sum(List.fill(x)(number)) == x * number ")
    println("빈 리스트를 넣으면 0 => sum(List[Int]()) == 0")

    println("== 연습문제 8.2 == List[Int]의 최댓값을 찾는 함수를 명시하는 속성들은? ")
    println("빈 리스트를 넣으면 0 또는 에러 발생")
    println("값이 하나뿐인 List 라면 그 값이 최대값이다.")
    println("뒤집어서 최대값을 찾아도 같은 값이 나와야 한다 => max(xs) == max(reverse(xs))")

    println("== 연습문제 8.3 == &&를 Prop의 한 메서드로서 구현하라.")
    println("???")

    println("== 연습문제 8.4 == choose ")
    println()

    println("== 연습문제 8.5 == unit, boolean, listOfN")

    println("== 연습문제 8.6 == ")
    println()

    println("== 연습문제 8.7 == ")
    println()

    println("== 연습문제 8.8 == ")
    println()

    println("== 연습문제 8.9 == ")
    println()

    println("== 연습문제 8.10 == ")
    println()

    println("== 연습문제 8.11 == ")
    println()

    println("== 연습문제 8.12 == ")
    println()

  }
}

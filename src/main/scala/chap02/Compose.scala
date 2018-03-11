package chap02

// 연습문제 2.5
object Compose {

  /*
  A를 넣으면 C로 바꾸고 싶다.
  g 함수에 A를 넣으면 B가 나온다.
  f 함수에 B를 넣으면 C가 나온다.
  f 함수에 g 함수 결과를 넣으면 된다.
   */
  def compose[A, B, C](f: B => C, g: A => B): A => C =
    (a: A) => f(g(a))

}

package chap02

// 연습문제 2.3
object Curry {

  /*
  A를 넣으면 B를 C로 바꾸는 함수가 필요하다.
  함수 f에 A, B를 넣으면 C가 나온다
  A는 B를 주고 B는 C를 준다.
  캡쳐링을 이용해서 a, b를 f에 넣어보자
   */
  def curry[A, B, C](f: (A, B) => C): A => (B => C) =
    (a: A) => (b: B) => f(a: A, b: B)

}

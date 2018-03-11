package chap02

// 연습문제 2.4
object UnCurry {

  /*
  A, B를 넣으면 C로 바꾸고 싶다.
  함수 f는 A를 B로 B를 C로 바꾼다.

   */
  def uncurry[A, B, C](f: A => B => C): (A, B) => C =
    (a: A, b: B) => f(a: A)(b: B)
//  (a: A, b: B) => f(a: A).apply(b: B)

}

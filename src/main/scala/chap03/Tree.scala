package chap03

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  def size[A](t: Tree[A]): Int =
    t match {
      case Leaf(_) => 1
      case Branch(left, right) => size(left) + size(right) + 1
    }

  def maximum(t: Tree[Int]): Int =
    t match {
      case Leaf(value) => value
      case Branch(left, right) => maximum(left) max maximum(right)
    }

  def depth(t: Tree[Int]): Int =
    t match {
      case Leaf(_) => 0;
      case Branch(left, right) => (depth(left) max depth(right)) + 1
    }

  def map[A,B](t: Tree[A])(f: A => B): Tree[B] =
    t match {
      case Leaf(a) => Leaf(f(a));
      case Branch(left, right) => Branch(map(left)(f), map(right)(f))
    }

  def fold[A,B](t: Tree[A])(f: A => B)(g: (B, B) => B): B =
    t match {
      case Leaf(a) => f(a)
      case Branch(left, right) => g(fold(left)(f)(g), fold(right)(f)(g))
    }

  def sizeByFold[A](t: Tree[A]): Int =
    fold(t)(_ => 1)((left, right) => left + right + 1)

  def maximumByFold(t: Tree[Int]): Int =
    fold(t)(x => x)((left, right) => left max right)

  def depthByFold(t: Tree[Int]): Int =
    fold(t)(_ => 0)((left, right) => (left max right) + 1)

  // *: Tree[B] 타입 추론이 왜 안되지
  def mapByFold[A,B](t: Tree[A])(f: A => B): Tree[B] =
    fold(t)(a => Leaf(f(a)): Tree[B])((left, right) => Branch(left, right))


  def main(args: Array[String]): Unit = {
    println("== 연습문제 3.25 == leaf와 branch의 size")
    println(size(Branch(Leaf(3), Leaf(5)))) // 3

    println("== 연습문제 3.26 == maximum")
    println(maximum(Branch(Branch(Leaf(1), Leaf(6)), Branch(Leaf(10), Leaf(44))))) // 44

    println("== 연습문제 3.27 == depth")
    println(depth(Leaf(0))) // 0
    println(depth(Branch(Leaf(1), Leaf(1)))) // 1
    println(depth(Branch(Branch(Leaf(2), Leaf(2)), Branch(Branch(Leaf(3), Branch(Leaf(4), Leaf(4))), Leaf(2))))) // 4

    println("== 연습문제 3.28 == map")
    println(map(Branch(Leaf(1), Leaf(2)))(_ + " string")) // Branch(Leaf(1 string),Leaf(2 string))
    // 이거 왜 밑줄 못쓰지

    println("== 연습문제 3.29 == fold")
    println(sizeByFold(Branch(Leaf(3), Leaf(5)))) // 3
    println(maximumByFold(Branch(Branch(Leaf(1), Leaf(6)), Branch(Leaf(10), Leaf(44))))) // 44
    println(depthByFold(Leaf(0))) // 0
    println(depthByFold(Branch(Leaf(1), Leaf(1)))) // 1
    println(depthByFold(Branch(Branch(Leaf(2), Leaf(2)), Branch(Branch(Leaf(3), Branch(Leaf(4), Leaf(4))), Leaf(2))))) // 4
    println(mapByFold(Branch(Leaf(1), Leaf(2)))(_ + " string")) // Branch(Leaf(1 string),Leaf(2 string))

  }
}

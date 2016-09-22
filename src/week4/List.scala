package week4

trait List[+T] {
  def isEmpty: Boolean
  def head: T
  def tail: List[T]
}

class Cons[T](val head: T, val tail: List[T]) extends List[T] {
  def isEmpty = false
}

object Nil[T] extends List[Nothing] {
  def isEmpty: Boolean = true
  def head: Nothing = throw new NoSuchElementException("Nil.head")
  def tail: Nothing = throw new NoSuchElementException("Nil.tail")
}

object List extends List[T] {
  def apply[T](): List[T] = new Nil
  def apply(a: T): List[T] = new Cons(a, new Nil)
  def apply(a: T, b: T): List[T] = new Cons(a, new Cons(b, new Nil))
}

object test {
  val x: List[String] = Nil
  def isort(xs: List[int]): List[int] = xs match {
    case list() => List()
    case y :: ys => insert(y, isort(ys))
  }

  def insert(x, xs: List[int]): List[int] = xs match {
    case list() => List(x)
    case y :: ys => if (x <= y) x :: xs else insert(x, ys)
  }
}

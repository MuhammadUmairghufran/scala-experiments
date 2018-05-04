trait IntSet {
  def contains(x: Int): Boolean

  def include(x: Int): IntSet

  def union(that: IntSet): IntSet

  def map(f: Int => Int): IntSet
}

case object Empty extends IntSet {
  def contains(x: Int) =
    false

  def include(x: Int): IntSet =
    NonEmpty(x, Empty, Empty)

  override def toString = "."

  def union(that: IntSet) =
    that

  def map(f: Int => Int): IntSet =
    this
}

case class NonEmpty(el: Int, left: IntSet, right: IntSet) extends IntSet {
  def contains(x: Int) =
    if (x < el) left.contains(x)
    else if (x > el) right contains x
    else true

  def include(x: Int) =
    if (x < el) NonEmpty(el, left include x, right)
    else if (x > el) NonEmpty(el, left, right include x)
    else this

  override def toString = "{" + left + el + right + "}"

  def union(that: IntSet) =
    ((left union that) union right) include el

  def map(f: Int => Int) =
    (left map f) union (right map f) include f(el)

}

val is1 = Empty include 7 include 5 include 12 include 9 include 15
val is2 = Empty include 8 include 13

is1 map (x => -x)


sealed trait List[+A] {
  def ++[B >: A](ys: List[B]): List[B] = this match {
    case Nil => ys
    case Cons(head, tail) => Cons(head, tail ++ ys)

  }
}

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A] {
  override def toString() = head + " " + tail
}

def foldRight[A, B](xs: List[A])(zero: B)(f: (A, B) => B): B = xs match {
  case Nil => zero
  case Cons(h, t) => f(h, foldRight(t)(zero)(f))
}

val list1 = Cons("Hello", Cons("World", Cons("!", Nil)))


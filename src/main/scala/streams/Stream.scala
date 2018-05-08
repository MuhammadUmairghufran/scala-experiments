package streams

sealed trait Stream[+A] {
  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, t) => Some(h())
  }

  def take(n: Int): Stream[A] = {
    println("n", this)
    this match {
      case Cons(h, t) if n > 1 => Stream.cons(h(), t().take(n - 1))
      case Cons(h, _) if n == 1 => Stream.cons(h(), Stream.empty)
      case _ => Stream.empty
    }
  }

  def toList: List[A] = {
    @annotation.tailrec
    def move(s: Stream[A], acc: List[A]): List[A] = s match {
      case Cons(h, t) => move(t(), h() :: acc)
      case _ => acc
    }

    move(this, List()).reverse
  }
}

case object Empty extends Stream[Nothing]

case class Cons[+A](head: () => A, tail: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](h: => A, t: => Stream[A]): Stream[A] = {
    lazy val head = h
    lazy val tail = t
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](args: A*): Stream[A] =
    if (args.isEmpty) empty
    else cons(args.head, apply(args.tail: _*))
}
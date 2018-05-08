package streams

sealed trait Stream[+A] {
  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, t) => Some(h)
  }

  def take(n: Int): Stream[A] = {
    this match {
      case Cons(h, t) if n > 1 => Stream.cons(h, t.take(n - 1))
      case Cons(h, _) if n == 1 => Stream.cons(h, Stream.empty)
      case _ => Stream.empty
    }
  }

  def drop(n: Int): Stream[A] = {
    this match {
      case Cons(h, t) if n >= 1 => t.drop(n - 1)
      case Cons(h, _) if n == 0 => this
      case _ => Stream.empty
    }
  }

  def map[B](f: A => B): Stream[B] = this match {
    case Cons(h, t) => Stream.cons(f(h), t.map(f))
    case _ => Stream.empty
  }

  def existsSimple(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => p(h) || t.existsSimple(p)
    case _ => false
  }

  def foldRight[B](acc: B)(f: (A, B) => B): B = this match {
    case Cons(h, t) => f(h, t.foldRight(acc)(f))
    case _ => acc
  }

  def exists(func: A => Boolean): Boolean =
    foldRight(false)((element, acc) => func(element) || acc)

  def forAll(func: A => Boolean): Boolean =
    foldRight(acc = true)((element, acc) => func(element) && acc)

  def toList: List[A] = {
    @annotation.tailrec
    def move(s: Stream[A], acc: List[A]): List[A] = s match {
      case Cons(h, t) => move(t, h :: acc)
      case _ => acc
    }

    move(this, List()).reverse
  }
}

case object Empty extends Stream[Nothing]

case class Cons[+A](head: A, tail: Stream[A]) extends Stream[A]

object Stream {
  def cons[A](h: => A, t: => Stream[A]): Stream[A] = {
    lazy val head = h
    lazy val tail = t
    Cons(head, tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](args: A*): Stream[A] =
    if (args.isEmpty) empty
    else cons(args.head, apply(args.tail: _*))

  // temp to 100 because of call by value
  def from(n: Int): Stream[Int] = if (n < 100) cons(n, from(n + 1)) else empty

  //def from(n: Int): Stream[Int] = cons(n, from(n+1))
}
package monoid_examples

import parallel.parallel

object foldMap {

  def foldMapSegment[A, B](xs: IndexedSeq[A], from: Int, to: Int,
                           m: Monoid[B])
                          (f: A => B): B = {
    var res = f(xs(from))
    var index = from + 1
    while (index < to) {
      res = m.op(res, f(xs(index)))
      index = index + 1
    }
    res
  }

  def foldMapPar[A, B](xs: IndexedSeq[A], from: Int, to: Int, m: Monoid[B])
                      (f: A => B)
                      (implicit theresholdSize: Int): B = {
    if (to - from <= theresholdSize) foldMapSegment(xs, from, to, m)(f)
    else {
      val middle = from + (to - from) / 2
      val (l, r) = parallel(
        foldMapPar(xs, from, middle, m)(f)(theresholdSize),
        foldMapPar(xs, middle, to, m)(f)(theresholdSize))
      m.op(l, r)
    }
  }
}

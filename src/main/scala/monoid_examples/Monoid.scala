package monoid_examples

trait Monoid[A] {
  def op(x: A, y: A): A
  def zero: A
}
package monoid_examples

import monoid_examples.foldMap.foldMapPar

import scala.util.Random

object MonoidExamples {

  def main(args: Array[String]): Unit = {
    sumSequenceOfSquaresOfIntegers()
  }

  def sumSequenceOfSquaresOfIntegers() = {
    val rnd = new Random()
    val length = 100000
    val source = (0 until length).map(_ * rnd.nextInt()).toVector
    implicit val threshold: Int = 1000

    val monoid = new Monoid[Int] {
      def op(x: Int, y: Int) = x + y
      def zero = 0
    }

    val result = foldMapPar(source, 0, source.length, monoid)(Math.pow(_, 2).toInt)
    println(s"result: $result")
  }
}

package monoid_examples

import monoid_examples.foldMap.foldMapPar

import scala.util.Random

object MonoidExamples {

  def main(args: Array[String]): Unit = {
    println(sumSequenceOfSquaresOfIntegers())
    println(sumAndNumberOfPositiveIntegers())
  }

  def sumSequenceOfSquaresOfIntegers(): Int = {
    val rnd = new Random()
    val length = 100000
    val source = (0 until length).map(_ * rnd.nextInt()).toVector
    implicit val threshold: Int = 1000

    val monoid = new Monoid[Int] {
      def op(x: Int, y: Int): Int = x + y

      def zero = 0
    }

    foldMapPar(source, 0, source.length, monoid)(Math.pow(_, 2).toInt)
  }

  def sumAndNumberOfPositiveIntegers(): (Int, Int) = {
    val rnd = new Random()
    val length = 1000000
    val source = (0 until length).map(n => {
      val x = rnd.nextInt(100)
      if (x % 2 == 0) x else -x
    }).toVector
    implicit val threshold: Int = 1000

    val monoid = new Monoid[(Int, Int)] {
      def op(x: (Int, Int), y: (Int, Int)): (Int, Int) = {
        val x_ = if (x._1 >= 0) x else zero
        val y_ = if (y._1 >= 0) y else zero
        (x_._1 + y_._1, x_._2 + y_._2)
      }

      def zero: (Int, Int) = (0, 0)
    }
    foldMapPar[Int, (Int, Int)](source, 0, source.length, monoid)((_, 1))
  }
}

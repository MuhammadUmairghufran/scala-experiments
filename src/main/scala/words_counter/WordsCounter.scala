package words_counter

import scala.io.Source
import monoid_examples.Monoid
import monoid_examples.foldMap.{foldMapPar, foldMapSegment}
import org.scalameter.{Key, Warmer, config}

object WordsCounter {
  def main(args: Array[String]): Unit = {
    val source = readFile("big.txt")
    println(s"Count sequential:   ${wordsCounterSeq(source)}")
    println(s"Count parallel:     ${wordsCounter(source)}")
    println("*** Speed Measures ***")

    val standardConfig = config(
      Key.exec.minWarmupRuns -> 5,
      Key.exec.maxWarmupRuns -> 10,
      Key.exec.benchRuns -> 100,
      Key.verbose -> true
    ).withWarmer(new Warmer.Default)
    val seqTime = standardConfig.measure(wordsCounterSeq(source))
    val parTime = standardConfig.measure(wordsCounter(source))

    println(s"sequential time:  $seqTime")
    println(s"parallel time:    $parTime")
    println(s"speedup           ${seqTime.value / parTime.value}")
  }

  def readFile(filename: String): Vector[String] = {
    val bufferedSource = Source.fromFile(filename)
    val lines = (for (line <- bufferedSource.getLines) yield line).toVector
    bufferedSource.close()
    println(s"number of lines in $filename: ${lines.length}")
    lines
  }

  def lineWordsCounter(line: String): (Boolean, Int, Boolean) = {
    val regex = "[\\.,\\s!\\?;:()\\\"]+"
    val artEndOfLine = "|#|"
    val splitted = (line + "|#|").split(regex)
    val fullWordsLength = splitted.length - 2
    val leftPunctuation = splitted.head == ""
    val rightPunctuation = splitted.last == artEndOfLine

    (!leftPunctuation, fullWordsLength, !rightPunctuation)
  }

  def makeMonoid(): Monoid[(Boolean, Int, Boolean)] = new Monoid[(Boolean, Int, Boolean)] {
    def op(x: (Boolean, Int, Boolean), y: (Boolean, Int, Boolean)): (Boolean, Int, Boolean) = {
      val hasUnfinishedOnLeft = x._1
      val hasUnfinishedOnRight = y._3
      val hasUnfinishedBetween = x._3 || y._1
      (hasUnfinishedOnLeft, x._2 + y._2 + (if (hasUnfinishedBetween) 1 else 0), hasUnfinishedOnRight)
    }

    def zero: (Boolean, Int, Boolean) = (false, 0, false)
  }

  private def combine(x: (Boolean, Int, Boolean)): Int = x._2 + (if (x._1) 1 else 0) + (if (x._3) 1 else 0)

  def wordsCounterSeq(source: Vector[String]): Int = {
    val monoid = makeMonoid()
    combine(foldMapSegment[String, (Boolean, Int, Boolean)](source, from = 0, source.length, monoid)(lineWordsCounter))
  }

  def wordsCounter(source: Vector[String]): Int = {
    implicit val threshold: Int = 1000
    val monoid = makeMonoid()
    combine(foldMapPar[String, (Boolean, Int, Boolean)](source, from = 0, source.length, monoid)(lineWordsCounter))
  }
}

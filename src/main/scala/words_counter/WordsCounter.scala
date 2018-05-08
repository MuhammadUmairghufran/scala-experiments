package words_counter

import scala.io.Source
import monoid_examples.Monoid
import monoid_examples.foldMap.{foldMapPar, foldMapSegment}
import org.scalameter.{Key, Warmer, config}

case class LineCounter(leftUnfinished: Boolean, count: Int, rightUnfinished: Boolean)

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

  def lineWordsCounter(line: String): LineCounter = {
    val regex = "[\\.,\\s!\\?;:()\\\"]+"
    val artEndOfLine = "|#|"
    val splitted = (line + "|#|").split(regex)
    val fullWordsLength = splitted.length - 2
    val leftPunctuation = splitted.head == ""
    val rightPunctuation = splitted.last == artEndOfLine

    LineCounter(!leftPunctuation, fullWordsLength, !rightPunctuation)
  }

  def makeMonoid(): Monoid[LineCounter] = new Monoid[LineCounter] {
    def op(x: LineCounter, y: LineCounter): LineCounter = {
      val hasUnfinishedBetween = x.rightUnfinished || y.leftUnfinished
      LineCounter(x.leftUnfinished, x.count + y.count + (if (hasUnfinishedBetween) 1 else 0), y.rightUnfinished)
    }

    def zero: LineCounter = LineCounter(leftUnfinished = false, 0, rightUnfinished = false)
  }

  private def combine(x: LineCounter): Int = x.count + (if (x.leftUnfinished) 1 else 0) + (if (x.rightUnfinished) 1 else 0)

  def wordsCounterSeq(source: Vector[String]): Int = {
    val monoid = makeMonoid()
    combine(foldMapSegment[String, LineCounter](source, from = 0, source.length, monoid)(lineWordsCounter))
  }

  def wordsCounter(source: Vector[String]): Int = {
    implicit val threshold: Int = 1000
    val monoid = makeMonoid()
    combine(foldMapPar[String, LineCounter](source, from = 0, source.length, monoid)(lineWordsCounter))
  }
}

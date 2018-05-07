package words_counter

import monoid_examples.Monoid
import monoid_examples.foldMap.{foldMapPar, foldMapSegment}
import org.scalameter.{Key, Warmer, config}

object WordsCounter {
  def main(args: Array[String]): Unit = {
    val text51 = Vector("As in previous year, the event this not strictly a race against each other,",
      " this is race against the clock, as the cars are released at one-min",
      "ute intervals with the larger professional class cars go",
      "ing before the slower cars, in the Mille Miglia, however ",
      "the smaller displacement slower cars started first."
    )
    println(s"Count sequential:   ${wordsCounterSeq(text51)}")
    println(s"Count parallel:     ${wordsCounter(text51)}")
    println("*** Speed Measures ***")

    val standardConfig = config(
      Key.exec.minWarmupRuns -> 10,
      Key.exec.maxWarmupRuns -> 30,
      Key.exec.benchRuns -> 50,
      Key.verbose -> true
    ).withWarmer(new Warmer.Default)
    val seqTime = standardConfig.measure(wordsCounterSeq(text51))
    val parTime = standardConfig.measure(wordsCounter(text51))

    println(s"sequential time:  $seqTime")
    println(s"parallel time:    $parTime")
    println(s"speedup           ${seqTime.value / parTime.value}")
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
    implicit val threshold: Int = 2
    val monoid = makeMonoid()
    combine(foldMapSegment[String, (Boolean, Int, Boolean)](source, from = 0, source.length, monoid)(lineWordsCounter))
  }

  def wordsCounter(source: Vector[String]): Int = {
    implicit val threshold: Int = 2
    val monoid = makeMonoid()
    combine(foldMapPar[String, (Boolean, Int, Boolean)](source, from = 0, source.length, monoid)(lineWordsCounter))
  }
}

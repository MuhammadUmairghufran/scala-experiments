package words_counter

import monoid_examples.Monoid
import monoid_examples.foldMap.foldMapPar

object WordsCounter {
  def main(args: Array[String]): Unit = {
    val text51 = Vector("As in previous year, the event this not strictly a race against each other,",
      " this is race against the clock, as the cars are released at one-min",
      "ute intervals with the larger professional class cars go",
      "ing before the slower cars, in the Mille Miglia, however ",
      "the smaller displacement slower cars started first."
    )
    println("Count:", wordsCounter(text51))
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

  def wordsCounter(source: Vector[String]): Int = {
    implicit val threshold: Int = 2
    val monoid = makeMonoid()

    val (onLeft, count, onRight) =
      foldMapPar[String, (Boolean, Int, Boolean)](source, from = 0, source.length, monoid)(lineWordsCounter)
    count + (if (onLeft) 1 else 0) + (if (onRight) 1 else 0)
  }
}

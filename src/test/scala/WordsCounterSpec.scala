import org.scalatest.FunSpec
import words_counter.WordsCounter

class WordsCounterSpec extends FunSpec {
  describe("line words counter") {

    val allSeparators = Array(" . ", ".", "...", ",", "  ", ")", "(", "?", "!", " ", ";", ":", "\"", "\n", "\t", "\r\n", " \n")

    it("should count words in the middle and add not finished marks when line wrapped without punctuation") {
      assert(WordsCounter.lineWordsCounter("hello great world") === (true, 1, true))
    }

    it("should count words in the middle and add not finished marks at the start") {
      assert(WordsCounter.lineWordsCounter("hello great world.") === (true, 2, false))
    }

    it("should count words in the middle and add not finished marks at the end") {
      assert(WordsCounter.lineWordsCounter(" hello great world") === (false, 2, true))
    }

    it("should count all words when they are surrounded with punctuation") {
      assert(WordsCounter.lineWordsCounter(" hello great world ") === (false, 3, false))
    }

    describe("punctuation marks - beginning") {
      it("should cover marks combination") {
        assert(WordsCounter.lineWordsCounter(". hello great world") === (false, 2, true))
      }

      it("should cover marks combination other way") {
        assert(WordsCounter.lineWordsCounter(" .hello great world") === (false, 2, true))
      }

      it("should cover marks duplication") {
        assert(WordsCounter.lineWordsCounter("..hello great world") === (false, 2, true))
      }

      it("should cover all punctuations mark in the beginning") {
        allSeparators.foreach(mark => {
          assert(WordsCounter.lineWordsCounter(mark + "hello great world") === (false, 2, true))
        })
      }
    }

    describe("punctuation marks - ending") {
      it("should cover marks combination") {
        assert(WordsCounter.lineWordsCounter("hello great world. ") === (true, 2, false))
      }

      it("should cover marks combination other way") {
        assert(WordsCounter.lineWordsCounter("hello great world .") === (true, 2, false))
      }

      it("should cover marks duplication") {
        assert(WordsCounter.lineWordsCounter("hello great world..") === (true, 2, false))
      }

      it("should cover all punctuations mark in the ending") {
        allSeparators.foreach(mark => {
          assert(WordsCounter.lineWordsCounter("hello great world" + mark) === (true, 2, false))
        })
      }
    }
  }

  describe("monoid") {
    val monoid = WordsCounter.makeMonoid()

    describe("unfinished words between lines") {
      it("should sum together two lines without not finished words") {
        assert(monoid.op((false, 3, false), (false, 5, false)) === (false, 8, false))
      }

      it("should sum together two lines when left has potentially unfinished word") {
        assert(monoid.op((false, 3, true), (false, 5, false)) === (false, 9, false))
      }

      it("should sum together two lines when right has potentially unfinished word") {
        assert(monoid.op((false, 3, false), (true, 5, false)) === (false, 9, false))
      }

      it("should sum together two lines when both have unfinished word in the middle") {
        assert(monoid.op((false, 3, true), (true, 5, false)) === (false, 9, false))
      }
    }

    describe("unfinished on the sides") {
      it("should sum together two lines with unfinished on left side") {
        assert(monoid.op((true, 3, false), (false, 5, false)) === (true, 8, false))
      }

      it("should sum together two lines with unfinished on right side") {
        assert(monoid.op((false, 3, false), (false, 5, true)) === (false, 8, true))
      }

      it("should sum together two lines with unfinished on right side and middle") {
        assert(monoid.op((false, 3, true), (false, 5, true)) === (false, 9, true))
      }

      it("should sum together two lines with unfinished everywhere") {
        assert(monoid.op((true, 3, true), (true, 5, true)) === (true, 9, true))
      }
    }
  }

  describe("words counter") {
    it("should count nothing") {
      assert(WordsCounter.wordsCounter(Vector("")) === 0)
    }

    it("should count one word") {
      assert(WordsCounter.wordsCounter(Vector("one")) === 1)
    }

    it("should count two words") {
      assert(WordsCounter.wordsCounter(Vector("one two")) === 2)
    }

    it("should count 3 words") {
      assert(WordsCounter.wordsCounter(Vector("one two three")) === 3)
    }

    it("should count 2 lines") {
      assert(WordsCounter.wordsCounter(Vector("one two three\n", "four five six")) === 6)
    }

    it("should count 2 lines that split one word") {
      assert(WordsCounter.wordsCounter(Vector("one two thr", "ee four five")) === 5)
    }

    it("should count punctuation on sides") {
      assert(WordsCounter.wordsCounter(Vector(" one two thr", "ee four five :)")) === 5)
    }

    it("should count more lines case") {
      val text51 = Vector("As in previous year, the event this not strictly a race against each other,",
        " this is race against the clock, as the cars are released at one-min",
        "ute intervals with the larger professional class cars go",
        "ing before the slower cars, in the Mille Miglia, however ",
        "the smaller displacement slower cars started first.")
      assert(WordsCounter.wordsCounter(text51) === 51)
    }
  }
}

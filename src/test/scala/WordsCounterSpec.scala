import org.scalatest.FunSpec
import words_counter.WordsCounter

class WordsCounterSpec extends FunSpec {
  describe("line words counter") {

    val allSeparators = Array(" . ", ".", "...", ",", "  ", "?", "!", " ", ";", ":", "\"", "\n", "\t", "\r\n", " \n")

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

    it("should ") {

    }
  }
}

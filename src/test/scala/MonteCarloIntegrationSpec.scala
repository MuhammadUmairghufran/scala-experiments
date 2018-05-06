import montecarlo.MonteCarloIntegration._
import org.scalactic.TolerantNumerics
import org.scalatest.FunSpec


class MonteCarloIntegrationSpec extends FunSpec {
  implicit val doubleEq = TolerantNumerics.tolerantDoubleEquality(0.03)
  val testPointsCount = 10000000

  def simpleFunction: Double => Double = x => x * x


  it("should throw an Exception if Xmin bigger than Xmax") {
    assertThrows[Exception] {
      integralSeq(simpleFunction, xMin = 3, xMax = 2, testPointsCount)
    }
  }

  describe("Correct calculation by different methods") {
    it("integralSeq should calculate simple integral") {
      assert(integralSeq(simpleFunction, xMin = 0, xMax = 1, testPointsCount) === 0.3333)
    }

    it("integralParallelSimple should calculate simple integral") {
      assert(integralParallelSimple(simpleFunction, xMin = 0, xMax = 1, testPointsCount) === 0.3333)
    }

    it("integralParallelRecursion should calculate simple integral") {
      assert(integralParallelRecursion(simpleFunction, xMin = 0, xMax = 1, testPointsCount) === 0.3333)
    }

    it("integralParallelList should calculate simple integral") {
      assert(integralParallelList(simpleFunction, xMin = 0, xMax = 1, testPointsCount) === 0.3333)
    }

    it("integralParallelTaskList should calculate simple integral") {
      assert(integralParallelTaskList(simpleFunction, xMin = 0, xMax = 1, testPointsCount) === 0.3333)
    }
  }

  describe("Different options of shifts and limits") {
    describe("x > 0") {
      it("should calculate integral with y > 0 and x > 0") {
        assert(integralSeq(x => x * x + 1, xMin = 0, xMax = 1, testPointsCount) === 1.3333)
      }

      it("should calculate integral with y < 0 and x > 0") {
        assert(integralSeq(x => x * x - 1, xMin = 0, xMax = 1, testPointsCount) === -0.6667)
      }

      it("should calculate integral with y from - to + and x > 0") {
        assert(integralSeq(x => x * x - 1, xMin = 0, xMax = 1.5, testPointsCount) === -0.375)
      }
    }

    describe("x < 0") {
      it("should calculate integral with y > 0 and x < 0") {
        assert(integralSeq(x => x * x + 1, xMin = -1, xMax = 0, testPointsCount) === 1.3333)
      }

      it("should calculate integral with y < 0 and x < 0") {
        assert(integralSeq(x => x * x - 1, xMin = -1, xMax = 0, testPointsCount) === -0.6667)
      }

      it("should calculate integral with y from - to + and x < 0") {
        assert(integralSeq(x => x * x - 1, xMin = -1.5, xMax = 0, testPointsCount) === -0.375)
      }
    }

    describe("x from - to +") {
      it("should calculate integral with y > 0") {
        assert(integralSeq(x => x * x + 1, xMin = -1, xMax = 2, testPointsCount) === 6.0)
      }

      it("should calculate integral with y < 0") {
        assert(integralSeq(x => x * x - 1, xMin = -1, xMax = 3, testPointsCount) === 5.33)
      }

      it("should calculate integral with y from - to +") {
        assert(integralSeq(x => x * x - 1, xMin = -1.5, xMax = 3, testPointsCount) === 5.625)
      }
    }
  }

  describe("More complex functions") {
    it("should calculate integral with more complex function") {
      assert(integralSeq(x => 2 * x * x + 4 * x * x * x, xMin = -2, xMax = 1.8, testPointsCount) === 3.71893)
    }

    it("should calculate integral for sin function") {
      assert(integralSeq(x => Math.sin(x), xMin = -0.9, xMax = 1.7, testPointsCount) === 0.750454)
    }

    it("should calculate integral for complex sin function") {
      assert(integralSeq(x => x * Math.sin(x * x), xMin = -2, xMax = 1, testPointsCount) === -0.59697)
    }
  }
}
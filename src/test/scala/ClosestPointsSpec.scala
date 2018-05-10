import closest_points_pair.ClosestPoints._
import closest_points_pair.{Point, PointsPair}
import org.scalactic.TolerantNumerics
import org.scalatest.FunSpec

class ClosestPointsSpec extends FunSpec {
  implicit val doubleEq = TolerantNumerics.tolerantDoubleEquality(0.0001)
  val points = Vector(Point(0, 0), Point(4, 3), Point(9, 6), Point(2, 3), Point(5, 10), Point(9, 1), Point(8, 7), Point(2, 9))

  it("should find zero euclidean distance") {
    assert(distance(Point(1, 1), Point(1, 1)) === 0.0)
  }

  it("should find non zero euclidean distance") {
    assert(distance(Point(1, 7), Point(8, 6)) === 7.07107)
  }

  it("should sort") {
    val sorted = Vector(Point(0, 0), Point(9, 1), Point(4, 3), Point(2, 3), Point(9, 6), Point(8, 7), Point(2, 9), Point(5, 10))
    assert(sortByY(points) === sorted)
  }

  it("should split by x for even length") {
    val (left, right, splitPoint) = splitByX(points)
    assert(left === Vector(Point(0, 0), Point(2, 3), Point(2, 9), Point(4, 3)))
    assert(right === Vector(Point(5, 10), Point(8, 7), Point(9, 6), Point(9, 1)))
    assert(splitPoint === Point(5, 10))
  }

  it("should split by x for odd length") {
    val (left, right, splitPoint) = splitByX(points :+ Point(2, 5))
    assert(left === Vector(Point(0, 0), Point(2, 3), Point(2, 9), Point(2, 5)))
    assert(right === Vector(Point(4, 3), Point(5, 10), Point(8, 7), Point(9, 6), Point(9, 1)))
    assert(splitPoint === Point(4, 3))
  }

  describe("boundary merge") {
    it("should make boundary merge when smallest in the left") {
      val left = Vector(Point(0, 0), Point(9, 1), Point(5, 3), Point(4, 3), Point(2, 3))
      val right = Vector(Point(9, 6), Point(8, 7), Point(5, 9), Point(2, 9), Point(5, 10))
      val leftDistance = bruteForceClosest(left).distance
      val rightDistance = bruteForceClosest(right).distance

      val finalDistance = boundaryMerge(left ++ right, leftDistance, rightDistance, right(0))
      assert(finalDistance === leftDistance)
    }

    it("should make boundary merge when smallest in the right") {
      val left = Vector(Point(0, 0), Point(9, 1), Point(7, 3), Point(4, 3), Point(2, 3))
      val right = Vector(Point(9, 6), Point(8, 7), Point(3, 9), Point(2, 9), Point(5, 10))
      val leftDistance = bruteForceClosest(left).distance
      val rightDistance = bruteForceClosest(right).distance

      val finalDistance = boundaryMerge(left ++ right, leftDistance, rightDistance, right(0))
      assert(finalDistance === rightDistance)
    }

    it("should make boundary merge when smallest in the middle") {
      val left = Vector(Point(0, 0), Point(9, 1), Point(8, 3), Point(5, 3), Point(2, 3))
      val right = Vector(Point(2, 4), Point(8, 7), Point(7, 9), Point(2, 9), Point(5, 10))
      val leftDistance = bruteForceClosest(left).distance
      val rightDistance = bruteForceClosest(right).distance

      val finalDistance = boundaryMerge(left ++ right, leftDistance, rightDistance, right(0))
      assert(finalDistance === distance(Point(2, 3), Point(2, 4)))
    }

    it("should make boundary merge for more interesting cases") {
      val left = Vector(Point(0, 0), Point(9, 1), Point(2, 3), Point(8, 3), Point(5, 3))
      val right = Vector(Point(8, 4), Point(2, 4), Point(8, 7), Point(7, 9), Point(2, 9), Point(5, 10))
      val leftDistance = bruteForceClosest(left).distance
      val rightDistance = bruteForceClosest(right).distance

      val finalDistance = boundaryMerge(left ++ right, leftDistance, rightDistance, right(0))
      assert(finalDistance === distance(Point(2, 3), Point(2, 4)))
    }

    it("should make boundary merge when have same y values in one of splits") {
      val left = Vector(Point(1, 2), Point(2, 3))
      val right = Vector(Point(8, 3), Point(5, 3))
      val leftDistance = bruteForceClosest(left).distance
      val rightDistance = bruteForceClosest(right).distance

      val finalDistance = boundaryMerge(left ++ right, leftDistance, rightDistance, right(0))
      assert(finalDistance === leftDistance)
    }
  }

  describe("brute force") {
    it("should calculate closest point correctly") {
      val bestPoint = bruteForceClosest(points)
      val actual = PointsPair(Point(9, 6), Point(8, 7))
      assert(bestPoint === actual)
      assert(bestPoint.distance === actual.distance)
    }
  }

  describe("divide and conquer") {
    it("should be calculate distance for small amount of points") {
      val bestPointDistance = divideAndConquerClosest(Vector(Point(1, 2), Point(2, 3), Point(8, 3), Point(5, 3)))
      assert(bestPointDistance === distance(Point(1, 2), Point(2, 3)))
    }

    it("should calculate closest point correctly") {
      val bestPointDistance = divideAndConquerClosest(points)
      val actual = PointsPair(Point(9, 6), Point(8, 7))
      assert(bestPointDistance === actual.distance)
    }

    it("should find closest points for 3 points with big difference between") {
      val somePoints = generate(length = 3, max = 3000, seed = 1)
      val bestPointBruteForce = bruteForceClosest(somePoints)
      val bestPointDCDistance = divideAndConquerClosest(somePoints)

      assert(bestPointDCDistance === bestPointBruteForce.distance)
    }

    it("should find closest points for other tricky case") {
      val somePoints = generate(length = 6, max = 30, seed = 2)
      val bestPointBruteForce = bruteForceClosest(somePoints)
      val bestPointDCDistance = divideAndConquerClosest(somePoints)

      assert(bestPointDCDistance === bestPointBruteForce.distance)
    }

    it("should find closest points for one more tricky case") {
      val somePoints = generate(length = 9, max = 9 * 5, seed = 1)
      val bestPointBruteForce = bruteForceClosest(somePoints)
      val bestPointDCDistance = divideAndConquerClosest(somePoints)

      assert(bestPointDCDistance === bestPointBruteForce.distance)
    }

    it("should calculate closest point correctly for different amount of points") {
      for (j <- 1 until 5) {
        println(j, "Calculate closest point for different amount of points")
        for (i <- 2 until 500) {
          val largeAmountOfPoints = generate(length = i, max = i * 5 * j, seed = 1)

          val bestPointBruteForce = bruteForceClosest(largeAmountOfPoints)
          val bestPointDCDistance = divideAndConquerClosest(largeAmountOfPoints)

          assert(bestPointDCDistance === bestPointBruteForce.distance)
        }
      }
    }

    it("should calculate closest point correctly for large density of points for different seeds") {
      for (i <- 0 until 10) {
        val largeAmountOfPoints = generate(length = 4000, max = 10000, seed = i)

        val bestPointBruteForce = bruteForceClosest(largeAmountOfPoints)
        val bestPointDCDistance = divideAndConquerClosest(largeAmountOfPoints)

        println(i, "Brute force:", bestPointBruteForce.distance, "Divide and conquer:", bestPointDCDistance)
        assert(bestPointDCDistance === bestPointBruteForce.distance)
      }
    }
  }
}

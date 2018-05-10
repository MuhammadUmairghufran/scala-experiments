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

  it("should split by y for even length") {
    val (left, right, splitPoint) = splitByY(points)
    assert(left === Vector(Point(0, 0), Point(9, 1), Point(4, 3), Point(2, 3)))
    assert(right === Vector(Point(9, 6), Point(8, 7), Point(2, 9), Point(5, 10)))
    assert(splitPoint === Point(9, 6))
  }

  it("should split by y for odd length") {
    val (left, right, splitPoint) = splitByY(points :+ Point(2, 5))
    assert(left === Vector(Point(0, 0), Point(9, 1), Point(4, 3), Point(2, 3)))
    assert(right === Vector(Point(2, 5), Point(9, 6), Point(8, 7), Point(2, 9), Point(5, 10)))
    assert(splitPoint === Point(2, 5))
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
    it("should be infinity if only 1 point") {
      val bestPointDistance = divideAndConquerClosest(Vector(Point(1, 2)))
      assert(bestPointDistance === Double.MaxValue)
    }

    it("should be infinity for 2 points") {
      val bestPointDistance = divideAndConquerClosest(Vector(Point(1, 2), Point(2, 3), Point(8, 3), Point(5, 3)))
      assert(bestPointDistance === distance(Point(1, 2), Point(2, 3)))
    }

    it("should calculate closest point correctly") {
      val bestPointDistance = divideAndConquerClosest(points)
      val actual = PointsPair(Point(9, 6), Point(8, 7))
      assert(bestPointDistance === actual.distance)
    }

    it("should calculate closest point correctly for large amount of points") {
      val length = 4000
      for (i <- 0 until 10) {
        val largeAmountOfPoints = generate(length = length, max = 1000000, seed = i)
        assert(largeAmountOfPoints.length === length)

        val bestPointBruteForce = bruteForceClosest(largeAmountOfPoints)
        val bestPointDCDistance = divideAndConquerClosest(largeAmountOfPoints)

        println(i, "Brute force:", bestPointBruteForce.distance, "Divide and conquer:", bestPointDCDistance)
        assert(bestPointDCDistance === bestPointBruteForce.distance)
      }
    }
  }
}

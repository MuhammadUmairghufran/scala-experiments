import closest_points_pair.ClosestPoints.{distance, bruteForceClosest}
import closest_points_pair.{Point, PointsPair}
import org.scalactic.TolerantNumerics
import org.scalatest.FunSpec

class ClosestPointsSpec extends FunSpec {
  implicit val doubleEq = TolerantNumerics.tolerantDoubleEquality(0.0001)
  val points = Vector(Point(0,0), Point(4,3), Point(9,6), Point(2,3), Point(5,10), Point(9,1), Point(8,7), Point(2, 9))

  it("should find zero euclidean distance") {
    assert(distance(Point(1, 1), Point(1, 1)) === 0.0)
  }

  it("should find non zero euclidean distance") {
    assert(distance(Point(1, 7), Point(8, 6)) === 7.07107)
  }

  describe("brute force") {
    it("should calculate closest point correctly") {
      val bestPoint = bruteForceClosest(points)
      val actual = PointsPair(Point(9,6), Point(8,7))
      assert(bestPoint === actual)
      assert(bestPoint.distance === actual.distance)
    }
  }
}

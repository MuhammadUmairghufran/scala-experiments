package montecarlo

import org.scalameter._
import parallel._
import scala.util.Random

object MonteCarloIntegration {

  private def square(f: Double => Double, xMin: Double, xMax: Double, yMin: Double, yMax: Double): Double =
    (yMax - yMin) * (xMax - xMin)

  private def countPointsUnderCurve(f: Double => Double, xMin: Double, xMax: Double, yMin: Double, yMax: Double,
                                    totalNumberOfPoints: Int): Int = {
    val rndX = new Random()
    val rndY = new Random()

    @annotation.tailrec
    def simulate(hits: Int, pointsGenerated: Int): Int = {
      if (pointsGenerated >= totalNumberOfPoints)
        hits
      else {
        val x = xMin + rndX.nextDouble() * (xMax - xMin)
        val y = yMin + rndY.nextDouble() * (yMax - yMin)

        simulate(hits + (if (y < f(x)) 1 else 0), pointsGenerated + 1)
      }
    }

    simulate(0, 0)
  }

  private def getBounds(f: Double => Double, xMin: Double, xMax: Double) = {
    if (xMin > xMax)
      throw new Exception(s"xMin $xMin is bigger than xMax $xMax")

    val yMin = 0 //Math.min(f(xMin), f(xMax))
    val yMax = Math.max(f(xMin), f(xMax))
    val area = square(f, xMin = xMin, xMax = xMax, yMin = yMin, yMax = yMax)
    (yMin, yMax, area)
  }

  private def getIntegral(area: Double, pointsInside: Int, pointsTotal: Int) = area * pointsInside / pointsTotal

  def integralSeq(f: Double => Double, xMin: Double, xMax: Double, totalNumberOfPoints: Int): Double = {
    val (yMin, yMax, area) = getBounds(f, xMin = xMin, xMax = xMax)
    val points = countPointsUnderCurve(f, xMin = xMin, xMax = xMax, yMin = yMin, yMax = yMax, totalNumberOfPoints)
    getIntegral(area, points, totalNumberOfPoints)
  }

  def integralParallelSimple(f: Double => Double, xMin: Double, xMax: Double, totalNumberOfPoints: Int): Double = {
    val (yMin, yMax, area) = getBounds(f, xMin = xMin, xMax = xMax)
    val (p1, p2) = parallel(
      countPointsUnderCurve(f, xMin, xMax, yMin, yMax, totalNumberOfPoints / 2),
      countPointsUnderCurve(f, xMin, xMax, yMin, yMax, totalNumberOfPoints / 2)
    )
    getIntegral(area, p1 + p2, totalNumberOfPoints)
  }

  def integralParallelRecursion(f: Double => Double, xMin: Double, xMax: Double, totalNumberOfPoints: Int): Double = {
    val (yMin, yMax, area) = getBounds(f, xMin = xMin, xMax = xMax)
    val recursionThreshold = 1000000 / 8

    def runParallelRecursion(totalNumberOfPoints: Int): Int = {
      if (totalNumberOfPoints <= recursionThreshold)
        countPointsUnderCurve(f, xMin, xMax, yMin, yMax, totalNumberOfPoints / 2)
      else {
        val (pi1, pi2) = parallel(runParallelRecursion(totalNumberOfPoints / 2), runParallelRecursion(totalNumberOfPoints / 2))
        pi1 + pi2
      }
    }

    val points = runParallelRecursion(totalNumberOfPoints)
    getIntegral(area, points, totalNumberOfPoints)
  }

  def main(args: Array[String]): Unit = {
    val totalNumberOfPoints = 1000000
    val testPoints = 10000000
    val xMin = 0
    val xMax = 1

    def f: Double => Double = x => 2 * x * x + 4 * x * x * x

    def f1: Double => Double = x => 3 * x * x

    def f2: Double => Double = x => x * x

    def f3: Double => Double = x => x * Math.sin(x * x)

    val standardConfig = config(
      Key.exec.minWarmupRuns -> 50,
      Key.exec.maxWarmupRuns -> 200,
      Key.exec.benchRuns -> 100,
      Key.verbose -> true
    ).withWarmer(new Warmer.Default)

    val seqTime = standardConfig.measure(
      integralSeq(f, xMin = xMin, xMax = xMax, totalNumberOfPoints)
    )

    val parSimpleTime = standardConfig.measure(
      integralParallelSimple(f, xMin = xMin, xMax = xMax, totalNumberOfPoints)
    )

    val parRecursionTime = standardConfig.measure(
      integralParallelRecursion(f, xMin = xMin, xMax = xMax, totalNumberOfPoints)
    )

    println(s"Integrate[1, 2]  (3x^2) sequential:       ${integralSeq(f1, xMin = 1, xMax = 2, testPoints)}")
    println(s"Integrate[1, 2]  (x^2) sequential:        ${integralSeq(f2, xMin = 1, xMax = 2, testPoints)}")
    println(s"Integrate[1, 2]  (x*sin(x^2)) sequential: ${integralSeq(f3, xMin = -1, xMax = 2, testPoints)}")
    println("***")
    println(s"Integrate[-1, 2] (2x^2+4x^3) sequential:   ${integralSeq(f, xMin = -1, xMax = 2, testPoints)}")
    println(s"Integrate[-1, 2] (2x^2+4x^3) parallel Sim: ${integralParallelSimple(f, xMin = -1, xMax = 2, testPoints)}")
    println(s"Integrate[-1, 2] (2x^2+4x^3) parallel Rec: ${integralParallelSimple(f, xMin = -1, xMax = 2, testPoints)}")
    println(s"sequential time:         $seqTime")
    println(s"parallel simple time:    $parSimpleTime")
    println(s"parallel recursion time: $parRecursionTime")
    println(s"speedup simple:     ${seqTime.value / parSimpleTime.value}")
    println(s"speedup recursion:  ${seqTime.value / parRecursionTime.value}")
  }
}
package montecarlo

import org.scalameter._
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

  def integralSeq(f: Double => Double, xMin: Double, xMax: Double, totalNumberOfPoints: Int): Double = {
    val yMin = 0 //Math.min(f(xMin), f(xMax))
    val yMax = Math.max(f(xMin), f(xMax))
    if (xMin > xMax)
      throw new Exception(s"xMin $xMin is bigger than xMax $xMax")

    val area = square(f, xMin = xMin, xMax = xMax, yMin = yMin, yMax = yMax)
    val points = countPointsUnderCurve(f, xMin = xMin, xMax = xMax, yMin = yMin, yMax = yMax, totalNumberOfPoints)
    area * points / totalNumberOfPoints
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

    val seqtime = standardConfig.measure(
      integralSeq(f, xMin = xMin, xMax = xMax, totalNumberOfPoints)
    )

    println(s"Integrate[-1, 2] (2x^2+4x^3) sequential:  ${integralSeq(f, xMin = -1, xMax = 2, testPoints)}")
    println(s"Integrate[1, 2]  (3x^2) sequential:       ${integralSeq(f1, xMin = 1, xMax = 2, testPoints)}")
    println(s"Integrate[1, 2]  (x^2) sequential:        ${integralSeq(f2, xMin = 1, xMax = 2, testPoints)}")
    println(s"Integrate[1, 2]  (x*sin(x^2)) sequential: ${integralSeq(f3, xMin = -1, xMax = 2, testPoints)}")
    println(s"sequential time:  $seqtime")
  }
}
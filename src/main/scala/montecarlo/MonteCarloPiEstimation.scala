package montecarlo

import org.scalameter
import parallel.parallel

import scala.util.Random
import org.scalameter._

object MonteCarloPiEstimation {

  def pi(totalNumberOfPoints: Int): Double = 4.0 * countPointsInsideCircle(totalNumberOfPoints) / totalNumberOfPoints

  def countPointsInsideCircle(totalNumberOfPoints: Int): Int = {
    val rndX = new Random()
    val rndY = new Random()

    def simulate(hits: Int, pointsGenerated: Int): Int = {
      if (pointsGenerated >= totalNumberOfPoints)
        hits
      else {
        val x = rndX.nextDouble()
        val y = rndY.nextDouble()

        simulate(hits + (if (x*x + y*y <= 1) 1 else 0), pointsGenerated + 1)
      }
    }

    simulate(0, 0)
  }

  def piParallelX(totalNumberOfPoints: Int): Double = {
    val (pi1, pi2) = parallel(countPointsInsideCircle(totalNumberOfPoints/2), countPointsInsideCircle(totalNumberOfPoints/2))
    4.0 * (pi1 + pi2) / totalNumberOfPoints
  }

  def main(args: Array[String]): Unit = {
    val totalNumberOfPoints = 1000000

    val standardConfig = config(
      Key.exec.minWarmupRuns -> 50,
      Key.exec.maxWarmupRuns -> 300,
      Key.exec.benchRuns -> 100,
      Key.verbose -> true
    ).withWarmer(new scalameter.Warmer.Default)

    val seqtime = standardConfig.measure(
      pi(totalNumberOfPoints)
    )

    val partime = standardConfig.measure(
      piParallelX(totalNumberOfPoints)
    )

    println(s"pi sequential: ${pi(totalNumberOfPoints)}")
    println(s"pi parallel:   ${piParallelX(totalNumberOfPoints)}")
    println(s"sequential time $seqtime")
    println(s"parallel time   $partime")
    println(s"speedup: ${seqtime.value / partime.value}")
  }
}
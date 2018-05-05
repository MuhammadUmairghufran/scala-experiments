package montecarlo

import org.scalameter._
import parallel._
import scala.util.Random


object MonteCarloPiEstimation {

  private def getPi(squarePoints: Int, totalNumberOfPoints: Int): Double = 4.0 * squarePoints / totalNumberOfPoints

  def piSeq(totalNumberOfPoints: Int): Double = getPi(countPointsInsideCircle(totalNumberOfPoints), totalNumberOfPoints)

  private def countPointsInsideCircle(totalNumberOfPoints: Int): Int = {
    val rndX = new Random()
    val rndY = new Random()

    def simulate(hits: Int, pointsGenerated: Int): Int = {
      if (pointsGenerated >= totalNumberOfPoints)
        hits
      else {
        val x = rndX.nextDouble()
        val y = rndY.nextDouble()

        simulate(hits + (if (x * x + y * y <= 1) 1 else 0), pointsGenerated + 1)
      }
    }

    simulate(0, 0)
  }

  def piParallelSimple(totalNumberOfPoints: Int): Double = {
    val (pi1, pi2) = parallel(countPointsInsideCircle(totalNumberOfPoints / 2), countPointsInsideCircle(totalNumberOfPoints / 2))
    getPi(pi1 + pi2, totalNumberOfPoints)
  }

  def piParallelParList(totalNumberOfPoints: Int): Double = {
    val count = Runtime.getRuntime.availableProcessors() - 1

    def parallelTask = () => countPointsInsideCircle(totalNumberOfPoints / count)

    val results = List.fill(count)(parallelTask).par.map(x => x())
    getPi(results.sum, totalNumberOfPoints)
  }

  def piParallelTaskList(totalNumberOfPoints: Int): Double = {
    val count = Runtime.getRuntime.availableProcessors()

    def parallelTask = countPointsInsideCircle(totalNumberOfPoints / count)

    val taskList = List.fill(count - 1)(task(parallelTask))
    val mainTask = parallelTask
    getPi(taskList.map(t => t.join()).sum + mainTask, totalNumberOfPoints)
  }

  def main(args: Array[String]): Unit = {
    val totalNumberOfPoints = 1000000

    val standardConfig = config(
      Key.exec.minWarmupRuns -> 50,
      Key.exec.maxWarmupRuns -> 200,
      Key.exec.benchRuns -> 100,
      Key.verbose -> true
    ).withWarmer(new Warmer.Default)

    val seqtime = standardConfig.measure(
      piSeq(totalNumberOfPoints)
    )

    val parStime = standardConfig.measure(
      piParallelSimple(totalNumberOfPoints)
    )

    val parPLtime = standardConfig.measure(
      piParallelParList(totalNumberOfPoints)
    )

    val parTLtime = standardConfig.measure(
      piParallelTaskList(totalNumberOfPoints)
    )

    println(s"pi sequential:    ${piSeq(totalNumberOfPoints)}")
    println(s"pi parallel s:    ${piParallelSimple(totalNumberOfPoints)}")
    println(s"pi parallel pl:   ${piParallelParList(totalNumberOfPoints)}")
    println(s"pi parallel tl:   ${piParallelTaskList(totalNumberOfPoints)}")
    println(s"sequential time:  $seqtime")
    println(s"parallel s time:  $parStime")
    println(s"parallel pl time: $parPLtime")
    println(s"parallel tl time: $parTLtime")
    println(s"speedup s:  ${seqtime.value / parStime.value}")
    println(s"speedup pl: ${seqtime.value / parPLtime.value}")
    println(s"speedup tl: ${seqtime.value / parTLtime.value}")
  }
}
import module1.homework.collections.BallsCasket
object Main {
  def main(args: Array[String]): Unit = {
    println("Hello, World!")

  // Homework 3 (probability of taking white balls).
  val ballsCasketsCount = 10000
  val ballsCaskets: List[BallsCasket] = (1 to ballsCasketsCount).map(_ => new BallsCasket()).toList
  val mapping = ballsCaskets.map(ballsCasket => ballsCasket.isWhiteBall)
  val takenWhiteBallsNumber = mapping.count(takenWhiteBallsNumber => takenWhiteBallsNumber)
  val probabilisticDistributionOfWhiteBall = takenWhiteBallsNumber / ballsCasketsCount.toDouble
  println("Experiment with " + ballsCasketsCount + " caskets with black and white balls inside. Total probability of white balls to be taken from all this caskets is " +
    probabilisticDistributionOfWhiteBall + " or " + probabilisticDistributionOfWhiteBall*100 + "%.")
  }
}
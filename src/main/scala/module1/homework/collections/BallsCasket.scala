// Homework 3 (probability of taking white balls).
package module1.homework.collections

import module1.homework.collections.color.{black, white, color}
import scala.util.Random

class BallsCasket {
  private val numberOfBallsInCasket = 6

  private var casket : List[color] = List()
  casket = List.fill(3)(black)
  casket = casket:::List.fill(3)(white)

  def isWhiteBall: Boolean = {
    val randomizationObject = new Random()
    val takenBallColor: color = casket(randomizationObject.nextInt(numberOfBallsInCasket))
    takenBallColor == color.white
  }
}

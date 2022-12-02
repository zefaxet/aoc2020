package year2020

import java.util
import scala.collection.BitSet
import scala.language.postfixOps
import scala.util.matching.Regex

object DayFive extends AOC2020(5) {

  override protected var runAsTest: Boolean = false
  override protected var testInput: String = "FBFBBFFRLR\nBFFFBBFRRR\nFFFBBBFRRR\nBBFFBBFRLL"

  override def main(args: Array[String]): Unit = {
    run()
  }

  def toByte(a: Int, b: Int): Int = (a << 1) | b

  override def partOne: String = {

    (input split "\n" map ( s => {
      val (row, column) = (s map {
        case 'F' | 'L' => 0
        case 'B' | 'R' => 1
      }) splitAt 7
      (row reduce toByte) * 8 + (column reduce toByte)
    }) max) toString

  }

  override def partTwo: String = {
    val ids = input split "\n" map ( s => {
      val (row, column) = (s map {
        case 'F' | 'L' => 0
        case 'B' | 'R' => 1
      }) splitAt 7
      (row reduce toByte) * 8 + (column reduce toByte)
    }) sorted
    var found = -1
    for (current <- 0 to ((ids length) - 2)) {
      val currentId = ids(current)
      if (ids(current + 1) - currentId == 2) found = currentId + 1
    }
    found toString
  }
}

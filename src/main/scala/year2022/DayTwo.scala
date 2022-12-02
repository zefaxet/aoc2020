package year2022

import scala.language.postfixOps

object DayTwo extends AOC2022(2) {

  override protected var runAsTest: Boolean = false
  override protected var testInput: String = "A Y\nB X\nC Z"

  def main(args: Array[String]): Unit = {

    run()

  }

  override def partOne: String = {

    (input split "\n" map (_ split " ") map (_ map {
      case "A" | "X" => 1
      case "B" | "Y" => 2
      case "C" | "Z" => 3
    }) map (a => {
      if (a(0) == a(1)) {
        a(1) + 3
      } else if (a(0) == (a(1) + 1) % 3 || (a(0) == 3 && a(1) == 2)) {
        a(1)
      } else {
        a(1) + 6
      }
    }) sum) toString

  }

  override def partTwo: String = {

    ((input split "\n" map (s => {
      val pair = s split " "
      (pair(0) match {
        case "A" => 1
        case "B" => 2
        case "C" => 3
      }, pair(1))
    }) map {
      case (1, "X") => 3
      case (2, "X") => 1
      case (3, "X") => 2
      case (point, "Y") => 3 + point
      case (1, "Z") => 2 + 6
      case (2, "Z") => 3 + 6
      case (3, "Z") => 1 + 6
    }) sum) toString

  }

}

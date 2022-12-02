package year2022

import scala.collection.immutable
import scala.language.postfixOps
import scala.util.control.Breaks.{break, breakable}

object DayOne extends AOC2022(1) {

  override protected var runAsTest: Boolean = true
  override protected var testInput: String = "1000\n2000\n3000\n\n4000\n\n5000\n6000\n\n7000\n8000\n9000\n\n10000"

  def main(args: Array[String]): Unit = {

    run()

  }

  override def partOne: String = {

    (input split "\n\n" map (_ split '\n' map (_.toInt)) map (_.sum) max) toString

  }

  override def partTwo: String = {

    ((input split "\n\n" map (_ split '\n' map (_.toInt)) map (_.sum) sorted) takeRight 3 sum) toString

  }

}

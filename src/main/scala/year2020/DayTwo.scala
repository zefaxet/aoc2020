package year2020

import java.util.regex.Pattern

import scala.util.matching.Regex

object DayTwo extends AOC2020(2) {

  override protected var runAsTest: Boolean = false
  override protected var testInput: String = """1-3 a: abcde
                                               |1-3 b: cdefg
                                               |2-9 c: ccccccccc""".stripMargin

  def main(args: Array[String]): Unit = {
    run()
  }

  val parsePattern: Regex = "^(\\d+)-(\\d+) (\\w): (\\w+)$".r("minOccur", "maxOccur", "policy", "password")

  override def partOne: String = {
    input split '\n' map(s => {
      val parsed = parsePattern.findFirstMatchIn(s) get
      val minOccur = parsed group "minOccur" toInt
      val maxOccur = parsed group "maxOccur" toInt
      val policy = (parsed group "policy")(0)
      val password = parsed group "password"
      (minOccur to maxOccur contains (password count(_ equals policy)))
    }) count (b => b) toString
  }

  override def partTwo: String = {
    input split '\n' map(s => {
      val parsed = parsePattern.findFirstMatchIn(s) get
      val minOccur = parsed group "minOccur" toInt
      val maxOccur = parsed group "maxOccur" toInt
      val policy = (parsed group "policy")(0)
      val password = parsed group "password"
      (password(minOccur - 1) equals policy) ^ (password(maxOccur - 1) equals policy)
    }) count (b => b) toString
  }

}

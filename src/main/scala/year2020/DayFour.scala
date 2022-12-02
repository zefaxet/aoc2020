package year2020

import scala.language.postfixOps
import scala.util.matching.Regex

object DayFour extends AOC2020(4) {

  override protected var runAsTest: Boolean = false
  override protected var testInput: String =
    "iyr:2010 hgt:190cm hcl:#123abc ecl:brn byr:1919 eyr:2021 pid:0123456789"

  override def main(args: Array[String]): Unit = {
    run()
    //1920 to 2003 filter (f => !byr.matches(s"byr:${f toString}")) foreach(println)
  }

  override def partOne: String = {
    //input split ("\n\n") map ( _ replace("\n", " ") )
    input split "\n\n" count( s => (
      (s contains "byr") &&
      (s contains "iyr") &&
      (s contains "eyr") &&
      (s contains "hgt") &&
      (s contains "hcl") &&
      (s contains "ecl") &&
      (s contains "pid")
    )) toString
  }

  val byr: Regex = "byr:(19[2-9][0-9]|200[012])".r
  val iyr: Regex = "iyr:20(1[0-9]|20)".r
  val eyr: Regex = "eyr:20(2[0-9]|30)".r
  val hgtcm: Regex = "hgt:1([5-8][0-9]|9[0-3])cm".r
  val hgtin: Regex = "hgt:(59|6[0-9]|7[0-6])in".r
  val hcl: Regex = "hcl:#[0-9a-f]{6}".r
  val ecl: Regex = "ecl:(amb|blu|brn|gry|grn|hzl|oth)".r
  val pid: Regex = "pid:[0-9]{9}".r
  val cid: Regex = "cid:.*".r


  override def partTwo: String = {

    input split "\n\n" count (str => {
      val attrs = str split "[ \n]"
        var hasByr, hasIyr, hasEyr, hasHgt, hasHcl, hasEcl, hasPid = false
        var valid = true
        attrs foreach{
          case byr(x) => hasByr = true
          case iyr(x) => hasIyr = true
          case eyr(x) => hasEyr = true
          case hgtcm(x) => hasHgt = true
          case hgtin(x) => hasHgt = true
          case s@x if hcl.matches(s) => hasHcl = true
          case ecl(x) => hasEcl = true
          case s@x if pid.matches(s) => hasPid = true
          case s@x if cid.matches(s) =>
          case _ => valid = false
        }
      hasByr && hasIyr && hasEyr && hasHgt && hasHcl && hasEcl && hasPid && valid
    } ) toString

  }
}

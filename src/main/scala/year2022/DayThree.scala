package year2022

import scala.language.postfixOps

object DayThree extends AOC2022(3) {

  override protected var runAsTest: Boolean = false
  override protected var testInput: String =
//    "b\nb\nb\n"
    "vJrwpWtwJgWrhcsFMMfFFhFp\n" +
    "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL\n" +
    "PmmdzqPrVvPwwTWBwg\n" +
    "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn\n" +
    "ttgJtRGJQctTZtZT\n" +
    "CrZsJsPPZsGzwwsLwLmpwMDw"

  def main(args: Array[String]): Unit = {

    run()

  }

//  override def partOne: String = {
//
//    ((input split '\n'
//      map (s => s splitAt s.length / 2)
//      map (t => (t._1 toSet, t._2 toSet))
//      flatMap (t => t._1 intersect t._2)
//      ) map (_ toInt match {
//      case upper if 65 to 90 contains upper => upper - 38
//      case lower if 97 to 122 contains lower => lower - 96
//      case e => throw new Exception("Invalid character valued at " + e)
//    }) sum) toString
//
//  }

  override def partTwo: String = {

    (input split '\n'
      grouped 3 flatMap (group => group map (_ toSet) reduce (_ intersect _))
      map (_ toInt match {
        case upper if 65 to 90 contains upper => upper - 38
        case lower if 97 to 122 contains lower => lower - 96
        case e => throw new Exception("Invalid character valued at " + e)
      }) sum
    ) toString

  }

}

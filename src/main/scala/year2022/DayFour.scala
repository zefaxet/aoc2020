package year2022

import scala.language.postfixOps

object DayFour extends AOC2022(4) {

  override protected var runAsTest: Boolean = false
  override protected var testInput: String = "2-4,6-8\n2-3,4-5\n5-7,7-9\n2-8,3-7\n6-6,4-6\n2-6,4-8"

  def main(args: Array[String]): Unit = {

    run()

  }

  override def partOne: String = {

    input split '\n' map (s => {
      val arr = s split ',' map (r => {
        val range = r split '-'
        Range.inclusive(range(0) toInt, range(1) toInt) toSet
      })
      (arr(0), arr(1))
    }) count (t => {
      val combine = t._1 union t._2
      (combine equals t._1) || (combine equals t._2)
    }) toString

  }

  override def partTwo: String = {

    input split '\n' map (s => {
      val arr = s split ',' map (r => {
        val range = r split '-'
        Range.inclusive(range(0) toInt, range(1) toInt)
      })
      (arr(0), arr(1))
    }) count (t => {
      (t._1 contains t._2(0)) || (t._1 contains t._2.last) || (t._2 contains t._1(0)) || (t._2 contains t._1.last)
    }) toString

  }

}

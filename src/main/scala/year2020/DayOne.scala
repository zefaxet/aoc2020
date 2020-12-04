package year2020

import scala.collection.IterableOnce.iterableOnceExtensionMethods
import scala.collection.immutable
import scala.collection.immutable.TreeSet
import scala.util.control.Breaks.{break, breakable}

object DayOne extends AOC2020(1) {

  override protected var runAsTest: Boolean = false
  override protected var testInput: String = ""

  def main(args: Array[String]): Unit = {

    run()

  }

  override def partOne: String = {
    val nums = immutable.TreeSet from(input split '\n' to LazyList map(_ toInt) filter(_ <= 2000)) toList
    var rClamp = nums.length - 1
    var lClamp = 0
    breakable {
      while (true) {
        val r = nums(rClamp)
        val l = nums(lClamp)
        l + r match {
          case 2020 => break
          case n if n > 2020 => rClamp -= 1
          case n if n < 2020 => lClamp += 1
        }
      }
    }
    nums(rClamp) * nums(lClamp) toString
  }

  override def partTwo: String = {

    val tree = immutable.TreeSet from(input split '\n' map(_ toInt) filter(_ < 2000))
    val nums = tree.toList

    var theret = "";

    for (big <- nums) {
      val boundary = 2000 - big
      var second_i_opt = nums findLast(_ < boundary)
      if (second_i_opt isDefined) {
        var second_i = nums indexOf(second_i_opt get);
        for (i <- (1 to second_i) reverseIterator) {
          for (third_i <- 0 to second_i)
            if (big + nums(i) + nums(third_i) == 2020) return big * nums(i) * nums(third_i) toString
        }

      }
    }

    theret

  }

}

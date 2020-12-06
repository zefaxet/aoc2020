package year2020

object DaySix extends AOC2020(6){
  override protected var runAsTest: Boolean = false
  override protected var testInput: String = ""

  override def main(args: Array[String]): Unit = run()

  override def partOne: String = {
    (input split "\n\n" map (s => {
      val set = s replace("\n", "") toSet;
      set.count(s => true)
    } ) sum) toString
  }

  override def partTwo: String = {
    (input split "\n\n" map (s => {
      s split "\n" reduce((a, b) => a.intersect(b)) length
    }) sum) toString
  }
}

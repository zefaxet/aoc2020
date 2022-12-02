package year2020

import scala.language.postfixOps

object DayThree extends AOC2020(3) {

  def main(args: Array[String]): Unit = {
    run()
  }

  override protected var runAsTest: Boolean = false
  override protected var testInput: String = """..##.......
                                               |#...#...#..
                                               |.#....#..#.
                                               |..#.#...#.#
                                               |.#...##..#.
                                               |..#.##.....
                                               |.#.#.#....#
                                               |.#........#
                                               |#.##...#...
                                               |#...##....#
                                               |.#..#...#.#""".stripMargin

  var dx = 3

  override def partOne: String = {

    val grid = input split '\n' map ( s => {
      //println(s)
      s map (c => c != '.')
    } )
    var i = 0;
    val mod = (grid(0) length);
    grid count( s => {
      val tree = s(i)
      //println(s"$tree, $i")
      i = (i + dx) % mod
      tree
    }) toString

  }

  val part2slopes = Seq((1, 1), (3, 1), (5, 1), (7, 1), (1, 2))

  override def partTwo: String = {
    val grid = input split '\n' map ( s => {
      //println(s)
      s map (c => c != '.')
    } ) zipWithIndex;
    (part2slopes map( slope => {
      val newGrid = grid filter( pair => pair._2 % slope._2 == 0 ) map( _._1 )
      var i = 0;
      val mod = (newGrid(0) length);
      newGrid count( s => {
        val tree = s(i)
        i = (i + slope._1) % mod
        tree
      })
    }) product) toString
  }
}

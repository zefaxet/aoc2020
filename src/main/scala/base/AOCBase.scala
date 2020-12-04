package base

import java.net.{URL, HttpURLConnection}

abstract class AOCBase(day: Int, year: Int) {

  final val SESSION = ""

  protected var runAsTest: Boolean
  protected var testInput: String

  def main(args: Array[String]): Unit

  def partOne: String

  def partTwo: String

  def input: String = if (runAsTest) testInput else fetchInput

  def fetchInput: String = {

    val connection = (new URL(s"https://adventofcode.com/$year/day/$day/input")).openConnection.asInstanceOf[HttpURLConnection]
    connection.setRequestMethod("GET")
    connection.setRequestProperty("Cookie", s"session=$SESSION")
    val inputStream = connection.getInputStream
    val content = io.Source.fromInputStream(inputStream).mkString
    if (inputStream != null) inputStream.close()
    content

  }

  def run(): Unit = println(s"One:\t$partOne\nTwo:\t$partTwo")

}

package day05

import kamlib.{Reader, Wrapper}

object Main {

  def main(args: Array[String]): Unit = {

    val input: Array[Int] = Reader.readString("/input5.txt").split("[^\\d-]+").map(x => x.toInt)
    val solution: Day5 = new Day5

    println("Running program...")
    Wrapper(solution.opCodeRunner(input)).print()
    println("Program finished...")

  }

}

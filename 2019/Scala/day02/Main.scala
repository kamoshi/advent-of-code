package day02

import kamlib.{Reader, Wrapper}

object Main
{
  /** MAIN */
  def main(args: Array[String]): Unit =
  {

    val solution: Day2 = new Day2
    val input: Array[Int] = Reader.readString("/input1.txt").split("\\D+").map(x => x.toInt)
    val copy: Array[Int] = input.clone()
    copy(1) = 12
    copy(2) = 2

    println("Part 1")
    Wrapper(solution.solveP1(input)).print()
    println("Part 2")
    Wrapper(solution.solveP2(input)).print()
  }

}

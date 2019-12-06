package day01

import kamlib.{Reader, Wrapper}

class Day1 {

  /** Find fuel needed */
  def solveP1(input: List[Int]): Int =
    input.foldLeft (0) { (acc, i) => acc + (i/3-2)}

  /** Find fuel needed and recursive fuel */
  def solveP2(input: List[Int]): Int =
  {
    @scala.annotation.tailrec
    def findRecursiveFuel(fuel: Int, acc: Int): Int =
    {
      if (fuel <= 0) acc
      else findRecursiveFuel((fuel/3)-2, fuel+acc)
    }
    input.foldLeft (0) { (acc, i) => acc + findRecursiveFuel((i/3)-2, 0)}
  }

  /** MAIN */
  def main(args: Array[String]): Unit =
  {
    val input: List[Int] = Reader.readList("/input1.txt").map(_.toInt)

    println("Part 1")
    Wrapper(solveP1(input)).print()
    println("Part 2")
    Wrapper(solveP2(input)).print()
  }

}

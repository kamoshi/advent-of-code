package day01

import kamlib.{Reader, Wrapper}

class Day1 {

  /** Find fuel needed */
  def solveP1(input: List[Int]): Int =
    input.foldLeft (0) {(acc, i) => acc + (i/3-2)}

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

}

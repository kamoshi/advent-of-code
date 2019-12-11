package day11

import kamlib.{Reader, Wrapper}

object Main {

  def main(args: Array[String]): Unit = {
    // Initialize initial program memory
    val memory: Array[Long] = Array.ofDim[Long](1200)
    val input: Array[Long] = Reader.readString("/input11.txt").split("[^\\d-]+").map(x => x.toLong)
    for(i <- input.indices) { memory(i) = input(i)}
    val solution: Day11 = new Day11(memory)

    println("Part 1:")
    Wrapper(solution.solveP1()).print()
    println("Part 2:")
    Wrapper(solution.solveP2()).print()

  }
}

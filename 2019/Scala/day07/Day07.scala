package day07

import day07.intcode.Solver

class Day07(original: Array[Int])
{
  val phases: List[Array[Int]] = Array(0, 1, 2, 3, 4).permutations.toList
  def solveP1(): Int =
  {
    val solver = new Solver(original)
    phases.map(seq => solver.solveSequenceSingleRun(seq)).max
  }

  val phases2: List[Array[Int]] = Array(5, 6, 7, 8, 9).permutations.toList
  def solveP2(): Int =
  {
    val solver = new Solver(original)
    phases2.map(seq => solver.solveSequenceMultiRun(seq)).max
  }

}

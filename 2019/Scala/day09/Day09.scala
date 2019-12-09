package day09

import day09.intcode.Machine

class Day09(original: Array[Long])
{
  def solveP1(): Long =
  {
    val machine = new Machine(original)
    machine.runUntilIO().runInput(1)
    machine.runUntilIO().runOutput()
  }

  def solveP2(): Long =
  {
    val machine = new Machine(original)
    machine.runUntilIO().runInput(2)
    machine.runUntilIO().runOutput()
  }
}

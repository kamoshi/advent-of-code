package day11

class Day11(original: Array[Long])
{
  def solveP1(): Long =
  {
    val robot = new Robot(original)
    robot.run()
    robot.paintedPanels
  }

  def solveP2(): Unit =
  {
    val robot = new Robot(original, 1)
    robot.run()
    robot.printMap()
  }
}

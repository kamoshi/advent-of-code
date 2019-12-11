package day11

import day11.direction._
import intcode.{Finished, Input, Machine, Ready}

import scala.collection.mutable

class Robot(software: Array[Long], init: Int = 0) {
  private[this] val brain: Machine = new Machine(software)
  private[this] val map: mutable.HashMap[(Int, Int), Int] = new mutable.HashMap()
  private[this] var location: (Int, Int) = (0, 0)
  private[this] var direction: Direction = Up

  if (init != 0) map.addOne((0,0), init) // when starting on white

  def paintedPanels: Int = map.size

  def updateColor(point: (Int, Int), color: Int): Unit = map(point) = color

  def findColor(point: (Int, Int)): Int = map.getOrElse(point, 0)

  def nextLocation(point: (Int, Int), direction: Direction, command: Int): ((Int, Int), Direction) = {
    (point, direction, command) match {
      case ((x, y), Up, 0) => ((x-1, y), Left)
      case ((x, y), Up, 1) => ((x+1, y), Right)
      case ((x, y), Down, 0) => ((x+1, y), Right)
      case ((x, y), Down, 1) => ((x-1, y), Left)
      case ((x, y), Left, 0) => ((x, y-1), Down)
      case ((x, y), Left, 1) => ((x, y+1), Up)
      case ((x, y), Right, 0) => ((x, y+1), Up)
      case ((x, y), Right, 1) => ((x, y-1), Down)
      case _ => throw new Exception("Something went wrong")
    }
  }

  // This print function uses a naive approach of having stuff hardcoded for my output, so might require calibration
  def printMap(): Unit = {
    val matrix = Array.ofDim[Char](6, 46)
    map.foreach(tuple => {
      matrix(tuple._1._2+5)(tuple._1._1) = if (tuple._2 == 0) '.' else '#'
    })
    for (i <- matrix.indices.reverse; j <- matrix(0).indices) {
      print(matrix(i)(j))
      if (j == 0) println()
    }
    println()
  }

  @scala.annotation.tailrec
  final def run(): Unit = {
    brain.getState match {
      // Ready to run
      case Ready =>
        brain.run()
        this.run()

      // Finished the program
      case Finished => ()

      // Program requires input of some sorts
      case Input =>
        brain.enqueue(findColor(location))
        brain.run()
        updateColor(location, brain.output.toInt)  // 1st output -> color
        val (p, d) = nextLocation(location, direction, brain.output.toInt)  // 2nd output -> direction
        location = p
        direction = d
        this.run()
    }
  }

}

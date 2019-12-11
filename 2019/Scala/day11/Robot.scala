package day11

import day11.direction._
import intcode.{Finished, Input, Machine, Ready}

import scala.collection.mutable

class Robot(software: Array[Long], init: Int = 0) {
  private[this] val brain: Machine = new Machine(software)
  private[this] val map: mutable.HashMap[Point, Int] = new mutable.HashMap()
  private[this] var location: Point = Point(0, 0)
  private[this] var direction: Direction = Up

  if (init != 0) map.addOne(Point(0,0), init) // when starting on white

  private[this] var paintedPanelsCount: Int = 0
  def paintedPanels: Int = paintedPanelsCount

  def updateColor(point: Point, color: Int): Unit = {
    if (map.contains(point)) {
      map(point) = color
    }
    else {
      paintedPanelsCount += 1
      map.addOne(point, color)
    }
  }

  def findColor(point: Point): Int = if (map.contains(point)) map(point) else 0

  def nextLocation(location: Point, direction: Direction, output: Int): (Point, Direction) = {
    (direction, output) match {
      case (Up, 0) => (location.left, Left)
      case (Up, 1) => (location.right, Right)
      case (Down, 0) => (location.right, Right)
      case (Down, 1) => (location.left, Left)
      case (Left, 0) => (location.down, Down)
      case (Left, 1) => (location.up, Up)
      case (Right, 0) => (location.up, Up)
      case (Right, 1) => (location.down, Down)
      case _ => throw new Exception("Something went wrong")
    }
  }

  // This print function uses a naive approach of having stuff hardcoded for my output, so might require calibration
  def printMap(): Unit = {
    val matrix = Array.ofDim[Char](6, 46)
    map.foreach(tuple => {
      matrix(tuple._1.y+5)(tuple._1.x) = if (tuple._2 == 0) '.' else '#'
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

package day10

import scala.collection.mutable

case class Vector2(x: Int, y: Int) {

  def toVector(other: Vector2): Vector2 = Vector2(other.x - x, other.y - y)

  def GCD(a: Int = x, b: Int = y): Int = if (b == 0) a.abs else GCD(b, a % b)

  def minimal: Vector2 = { val d = GCD(); if (d==0) Vector2(0,0) else Vector2(x/GCD(), y/GCD())}

  def raycast(other: Vector2): Vector2 = toVector(other).minimal

  def raycastSet(all: mutable.HashSet[Vector2]): mutable.HashSet[Vector2] = {
    val output = new mutable.HashSet[Vector2]
    all.foreach(x => output.add(this.raycast(x)))
    output.remove(Vector2(0, 0))
    output
  }

  def distance: Double = math.sqrt(x^2 + y^2)

  def angleFromY: Double = -math.atan2(x, y)

  def +(other: Vector2):Vector2 = Vector2(x + other.x, y + other.y)
}
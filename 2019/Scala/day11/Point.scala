package day11

case class Point(x: Int, y: Int) {
  def up = Point(x, y+1)
  def down = Point(x, y-1)
  def left = Point(x-1, y)
  def right = Point(x+1, y)
}

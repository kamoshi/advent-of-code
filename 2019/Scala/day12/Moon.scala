package day12

class Moon(var x: Int, var y: Int, var z: Int) {
  var vx = 0
  var vy = 0
  var vz = 0

  def changeVelBy(other: Moon): Unit = {
    if (other.x < x) vx -= 1
    else if (other.x > x) vx += 1
    if (other.y < y) vy -= 1
    else if (other.y > y) vy += 1
    if (other.z < z) vz -= 1
    else if (other.z > z) vz += 1
  }

  def physStep(): Unit = {
    x += vx
    y += vy
    z += vz
  }

  def print(): Unit = println(s"pos=<x=$x, y=  $y, z= $z>, vel=<x= $vx, y= $vy, z= $vz>")

  def energy: Int = {
    val potential = x.abs + y.abs + z.abs
    val kinetic = vx.abs + vy.abs + vz.abs
    potential * kinetic
  }

  def kinetic: Int = vx.abs + vy.abs + vz.abs

  def posX: Int = x
  def posY: Int = y
  def posZ: Int = z
}

package day12

class Day12 {

  def step(array: Array[Moon], n: Int): Unit = {
    for (_ <- 0 until n) {
      for (i <- array.indices; j <- array.indices) {
        array(i).changeVelBy(array(j))
      }
      for (i <- array.indices) {
        array(i).physStep()
      }
    }
  }

  def print(array: Array[Moon]): Unit = {
    for (i <- array.indices) {
      array(i).print()
    }
  }

  def solveP1(): Int = {
    val array: Array[Moon] = Array(new Moon(-16, 15, -9), new Moon(-14, 5, 4), new Moon(2, 0, 6), new Moon(-3, 18, 9))

    step(array, 1000)
    array.foldLeft (0) {(a, x) => a + x.energy}
  }


  def solveP2(): Long = {
    val array1: Array[Moon] = Array(new Moon(-16, 15, -9), new Moon(-14, 5, 4), new Moon(2, 0, 6), new Moon(-3, 18, 9))
    val array2: Array[Moon] = Array(new Moon(-16, 15, -9), new Moon(-14, 5, 4), new Moon(2, 0, 6), new Moon(-3, 18, 9))
    val array3: Array[Moon] = Array(new Moon(-16, 15, -9), new Moon(-14, 5, 4), new Moon(2, 0, 6), new Moon(-3, 18, 9))

    @scala.annotation.tailrec
    def findFirstXRepeat(array: Array[Moon], n: Int = 1): Int = {
      step(array, 1)
      if (array(0).posX == -16 && array(1).posX == -14 && array(2).posX == 2 && array(3).posX == -3) n+1 // n+1 because we're looking for kinetic E=0
      else findFirstXRepeat(array, n+1)
    }

    @scala.annotation.tailrec
    def findFirstYRepeat(array: Array[Moon], n: Int = 1): Int = {
      step(array, 1)
      if (array(0).posY == 15 && array(1).posY == 5 && array(2).posY == 0 && array(3).posY == 18) n+1 // n+1 because we're looking for kinetic E=0
      else findFirstYRepeat(array, n+1)
    }

    @scala.annotation.tailrec
    def findFirstZRepeat(array: Array[Moon], n: Int = 1): Int = {
      step(array, 1)
      if (array(0).posZ == -9 && array(1).posZ == 4 && array(2).posZ == 6 && array(3).posZ == 9) n+1 // n+1 because we're looking for kinetic E=0
      else findFirstZRepeat(array, n+1)
    }

    @scala.annotation.tailrec
    def GCD(a: Long, b: Long): Long = if (b == 0) a.abs else GCD(b, a % b)

    def LCM(a: Long, b: Long): Long = (a * b).abs / GCD(a, b)

    val repX = findFirstXRepeat(array1)
    val repY = findFirstYRepeat(array2)
    val repZ = findFirstZRepeat(array3)

    LCM(repZ, LCM(repX, repY))
  }

}

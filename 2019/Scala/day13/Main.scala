package day13

import intcode.{Finished, Input, Machine, Ready}
import kamlib.{Reader, Wrapper}

import scala.collection.mutable

object Main {

  val screen = Array.ofDim[Int](50, 50)

  @scala.annotation.tailrec
  def drawScreen(list: List[Long]): Unit = {
    list match {
      case List() => ()
      case x1::x2::x3::xs =>
        if (x1 == -1 && x2 == 0) {
          println(x3)
          drawScreen(xs)
        }
        else drawScreen(xs) /*screen(x2.toInt)(x1.toInt) match {
          case 0 =>
            screen(x2.toInt)(x1.toInt) = x3.toInt
            drawScreen(xs)
          case 1 =>
            drawScreen(xs)
          case 2 =>
            if (x3 == 4) screen(x2.toInt)(x1.toInt) = x3.toInt
            else drawScreen(xs)
          case 3 =>
            drawScreen(xs)
          case 4 =>
            screen(x2.toInt)(x1.toInt) = x3.toInt
            drawScreen(xs)
        }*/
    }
  }

  def logScreen(): Unit = {
    println()
    for (i <- 0 until 50; j <- 0 until 50) {
      screen(j)(i) match {
        case 0 => print('.')
        case 1 => print('X')
        case 2 => print('#')
        case 3 => print('-')
        case 4 => print('o')
      }
      if (j == 49) println()
    }
  }

  @scala.annotation.tailrec
  final def listof0(n: Int, list: List[Long] = List()): List[Long] = {
    n match {
      case 0 => list
      case k => listof0(k-1, 0::list)
    }
  }

  @scala.annotation.tailrec
  final def iterate(machine: Machine): Unit = {
    machine.getState match {
      case Finished =>
        drawScreen(machine.outputAll)
        //logScreen()
        ()
      case Input =>
        machine.enqueue(0L)
        machine.run()
        drawScreen(machine.outputAll)
        //logScreen()
        iterate(machine)
      case Ready =>
        machine.run()
        drawScreen(machine.outputAll)
        //logScreen()
        iterate(machine)
    }
  }

  def main(args: Array[String]): Unit = {
    val memory: Array[Long] = Array.ofDim[Long](5000)
    val input: Array[Long] = Reader.readString("/input13.txt").split("[^\\d-]+").map(x => x.toLong)
    for(i <- input.indices) { memory(i) = input(i)}

    val machine: Machine = new Machine(memory)
    machine.setMem(0, 2)
    iterate(machine)

  }
}

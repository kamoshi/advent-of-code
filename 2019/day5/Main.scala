package day5

import day5.opcode.{Action, Jump, OpCode1, OpCode2, OpCode99}

import scala.io.Source

object Main {

  val insertTape = Source.fromFile(getClass.getResource("/input5.txt").getFile).mkString.split("[^\\d-]+").map(x => x.toInt)

  def opCodeRunner(tape: Array[Int], nextPtr: Int): Boolean =
  {
    //println("=======\nParsing at "+nextPtr+": ["+tape(nextPtr)+", "+tape(nextPtr+1)+", "+tape(nextPtr+2)+", "+tape(nextPtr+3)+ "]");
    val tuple = OpCode1.parseInt(tape(nextPtr));
    //println("Executing: " + tuple)
    tuple match
    {
      case (_, _, _, OpCode99) => true // finish
      case (m3, m2, m1, instruction: Jump) =>
        {
          val (bool, jmpPtr) = instruction.checkConditionAndJump(tape, tape(nextPtr+1), tape(nextPtr+2), tape(nextPtr+3), m1, m2, m3)
          //println("Jumping?: " + bool + " " + jmpPtr)
          if (bool) opCodeRunner(tape, jmpPtr)
          else opCodeRunner(tape, nextPtr + instruction.length)
        }
      case (m3, m2, m1, instruction: Action) =>
        {
          instruction.exec(tape, tape(nextPtr+1), tape(nextPtr+2), tape(nextPtr+3), m1, m2, m3)
          opCodeRunner(tape, nextPtr + instruction.length)
        }
    }
  }

  def main(args: Array[String]): Unit = {
    println("Starting program...")

    opCodeRunner(insertTape, 0)

    println("Program finished...")
  }
}

package day05

import day05.opcode.{Action, Jump, OpCode99}

class Day5
{
  @scala.annotation.tailrec
  final def opCodeRunner(tape: Array[Int], nextPtr: Int = 0): Boolean =
  {
    //println("=======\nParsing at "+nextPtr+": ["+tape(nextPtr)+", "+tape(nextPtr+1)+", "+tape(nextPtr+2)+", "+tape(nextPtr+3)+ "]");
    val tuple = OpCode99.parseInt(tape(nextPtr));
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
      case _ => throw new Exception("Something went wrong")
    }
  }

}

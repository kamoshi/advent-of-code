package day09.intcode.opcode

trait Jump extends OpCode
{
  /** Checks if the condition for jumping is fulfilled, returns boolean and jump pointer */
  def checkConditionAndJump(tape: Array[Long], relative: Long, param1: Long, param2: Long, param3: Long, mode1: Int, mode2: Int, mode3: Int): (Boolean, Int)
}


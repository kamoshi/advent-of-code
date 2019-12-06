package day05.opcode

trait Jump extends OpCode
{
  /** Checks if the condition for jumping is fulfilled, returns boolean and jump pointer */
  def checkConditionAndJump(tape: Array[Int], param1: Int, param2: Int, param3: Int, mode1: Int, mode2: Int, mode3: Int): (Boolean, Int)
}


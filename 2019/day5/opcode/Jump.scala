package day5.opcode

trait Jump extends OpCode
{
  /** Executes instruction for given parameters and modes */
  def checkConditionAndJump(tape: Array[Int], param1: Int, param2: Int, param3: Int, mode1: Int, mode2: Int, mode3: Int): (Boolean, Int)
}


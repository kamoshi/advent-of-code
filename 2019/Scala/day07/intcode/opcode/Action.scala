package day07.intcode.opcode

trait Action extends OpCode
{
  /** Executes instruction for given parameters and modes */
  def exec(tape: Array[Int], param1: Int, param2: Int, param3: Int, mode1: Int, mode2: Int, mode3: Int)
}

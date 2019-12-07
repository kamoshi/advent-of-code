package day07.intcode.opcode

trait Input extends OpCode
{
  /** Executes instruction for given parameters and modes */
  def input(tape: Array[Int], param1: Int, mode1: Int, input: Int): Unit
}

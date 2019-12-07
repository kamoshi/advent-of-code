package day07.intcode.opcode

trait Output extends OpCode
{
  /** Executes instruction for given parameters and modes */
  def output(tape: Array[Int], param1: Int, mode1: Int): Int
}

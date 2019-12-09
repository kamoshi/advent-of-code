package day09.intcode.opcode

trait Output extends OpCode
{
  /** Executes instruction for given parameters and modes */
  def output(tape: Array[Long], relative: Long, param1: Long, mode1: Int): Long
}

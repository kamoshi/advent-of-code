package intcode.opcode

trait Input extends OpCode
{
  /** Executes instruction for given parameters and modes */
  def input(tape: Array[Long], relative: Long, param1: Long, mode1: Int, input: Long): Unit
}

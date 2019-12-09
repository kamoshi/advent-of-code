package intcode.opcode

trait Action extends OpCode
{
  /** Executes instruction for given parameters and modes */
  def exec(tape: Array[Long], relative: Long, param1: Long, param2: Long, param3: Long, mode1: Int, mode2: Int, mode3: Int)
}

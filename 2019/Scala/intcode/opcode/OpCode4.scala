package intcode.opcode

/** Output
 *  Outputs the value of its only parameter.
 */
case object OpCode4 extends OpCode
{
  /** length of an instruction */
  override val length: Int = 2

  /** Executes instruction for given parameters and modes */
  def output(tape: Array[Long], relative: Long, param1: Long, mode1: Int): Long =
  {
    accessor(relative, param1, mode1, tape)
  }
}

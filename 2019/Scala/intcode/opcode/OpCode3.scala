package intcode.opcode

/** Input
 *  Takes a single integer as input and saves it to the position given by its only parameter.
 */
case object OpCode3 extends OpCode
{
  /** length of an instruction */
  override val length: Int = 2

  /** Executes instruction for given parameters and modes */
  def input(tape: Array[Long], relative: Long, param1: Long, mode1: Int, input: Long): Unit =
  {
    writer(tape, relative, param1, mode1, input)
  }
}

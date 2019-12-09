package intcode.opcode

/** Input */
case object OpCode3 extends Input
{
  /** length of an instruction */
  override val length: Int = 2

  /** Executes instruction for given parameters and modes */
  override def input(tape: Array[Long], relative: Long, param1: Long, mode1: Int, input: Long): Unit =
  {
    writer(tape, relative, param1, mode1, input)
  }
}

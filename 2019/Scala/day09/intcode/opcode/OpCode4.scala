package day09.intcode.opcode

/** Output */
case object OpCode4 extends Output
{
  /** length of an instruction */
  override val length: Int = 2

  /** Executes instruction for given parameters and modes */
  override def output(tape: Array[Long], relative: Long, param1: Long, mode1: Int): Long =
  {
    accessor(relative, param1, mode1, tape)
  }
}

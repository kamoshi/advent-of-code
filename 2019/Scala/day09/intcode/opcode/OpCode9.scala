package day09.intcode.opcode

case object OpCode9 extends OpCode
{
  /** Find what to add to relative */
  def exec(tape: Array[Long], relative: Long, param1: Long, mode1: Int): Long =
  {
    accessor(relative, param1, mode1, tape)
  }

  /** length of an instruction */
  override val length: Int = 2
}

package day07.intcode.opcode

/** Output */
case object OpCode4 extends Output
{
  /** length of an instruction */
  override val length: Int = 2

  /** Executes instruction for given parameters and modes */
  override def output(tape: Array[Int], param1: Int, mode1: Int): Int =
  {
    accessor(param1, mode1, tape)
  }
}

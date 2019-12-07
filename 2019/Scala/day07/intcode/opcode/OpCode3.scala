package day07.intcode.opcode

/** Input */
case object OpCode3 extends Input
{
  /** length of an instruction */
  override val length: Int = 2

  /** Executes instruction for given parameters and modes */
  override def input(tape: Array[Int], param1: Int, mode1: Int, input: Int): Unit =
  {
    //println(s"<< input: $input")
    writer(param1, 0, tape, input)
  }
}

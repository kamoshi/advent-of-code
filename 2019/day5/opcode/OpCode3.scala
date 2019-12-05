package day5.opcode

/** Input */
case object OpCode3 extends Action
{
  /** length of an instruction */
  override val length: Int = 2

  /** Executes instruction for given parameters and modes */
  override def exec(tape: Array[Int], param1: Int, param2: Int, param3: Int, mode1: Int, mode2: Int, mode3: Int): Unit =
  {
    println("<< input")
    val userInput = scala.io.StdIn.readInt()
    writer(param1, 0, tape, userInput)
  }
}

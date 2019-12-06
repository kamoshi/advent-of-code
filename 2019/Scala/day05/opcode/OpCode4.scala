package day05.opcode

/** Output */
case object OpCode4 extends Action
{
  /** length of an instruction */
  override val length: Int = 2

  /** Executes instruction for given parameters and modes */
  override def exec(tape: Array[Int], param1: Int, param2: Int, param3: Int, mode1: Int, mode2: Int, mode3: Int): Unit =
  {
    println(accessor(param1, mode1, tape))
  }
}

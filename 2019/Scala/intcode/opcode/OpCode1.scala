package intcode.opcode

/** Sum */
case object OpCode1 extends Action
{
  /** length of an instruction */
  override val length: Int = 4

  /** Executes instruction for given parameters and modes */
  override def exec(tape: Array[Long], relative: Long, param1: Long, param2: Long, param3: Long, mode1: Int, mode2: Int, mode3: Int): Unit =
  {
    val result = accessor(relative, param1, mode1, tape) + accessor(relative, param2, mode2, tape)
    writer(tape, relative, param3, mode3, result)
  }
}

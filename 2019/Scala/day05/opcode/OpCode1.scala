package day05.opcode

/** Sum */
case object OpCode1 extends Action
{
  /** length of an instruction */
  override val length: Int = 4

  /** Executes instruction for given parameters and modes */
  override def exec(tape: Array[Int], param1: Int, param2: Int, param3: Int, mode1: Int, mode2: Int, mode3: Int): Unit =
  {
    val result = accessor(param1, mode1, tape) + accessor(param2, mode2, tape)
    writer(param3, 0, tape, result) // Write mode is 0 default for now
  }
}

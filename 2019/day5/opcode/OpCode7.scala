package day5.opcode

case object OpCode7 extends Action
{
  /** length of an instruction */
  override val length: Int = 4

  /** Executes instruction for given parameters and modes */
  override def exec(tape: Array[Int], param1: Int, param2: Int, param3: Int, mode1: Int, mode2: Int, mode3: Int): Unit =
  {
    if (accessor(param1, mode1, tape) < accessor(param2, mode2, tape))
      writer(param3, 0, tape, 1)
    else
      writer(param3, 0, tape, 0)
  }
}

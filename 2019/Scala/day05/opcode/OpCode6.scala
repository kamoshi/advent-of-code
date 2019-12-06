package day05.opcode

case object OpCode6 extends Jump
{
  /** length of an instruction */
  override val length: Int = 3

  /** Executes instruction for given parameters and modes */
  override def checkConditionAndJump(tape: Array[Int], param1: Int, param2: Int, param3: Int, mode1: Int, mode2: Int, mode3: Int): (Boolean, Int) =
  {
    if (accessor(param1, mode1, tape) == 0) (true, accessor(param2, mode2, tape))
    else (false, 0)
  }
}

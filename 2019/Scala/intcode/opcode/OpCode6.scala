package intcode.opcode

case object OpCode6 extends Jump
{
  /** length of an instruction */
  override val length: Int = 3

  /** Executes instruction for given parameters and modes */
  override def checkConditionAndJump(tape: Array[Long], relative: Long, param1: Long, param2: Long, param3: Long, mode1: Int, mode2: Int, mode3: Int): (Boolean, Int) =
  {
    if (accessor(relative, param1, mode1, tape) == 0) (true, accessor(relative, param2, mode2, tape).toInt)
    else (false, 0)
  }
}

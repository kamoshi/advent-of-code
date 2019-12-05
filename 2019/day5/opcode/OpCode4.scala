package day5.opcode

/** Output */
case object OpCode4 extends Action
{
  /** length of an instruction */
  override val length: Int = 2

  /** Executes instruction for given parameters and modes */
  override def exec(tape: Array[Int], param1: Int, param2: Int, param3: Int, mode1: Int, mode2: Int, mode3: Int): Unit =
  {
    mode1 match
    {
      case 0 => println(accessor(param1, 0, tape))
      case 1 => println(param1)
    }
  }
}

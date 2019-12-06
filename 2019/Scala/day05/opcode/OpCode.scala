package day05.opcode

trait OpCode
{
  /** length of an instruction */
  val length: Int

  /** Parse int to full OpCode */
  def parseInt(code: Int): (Int, Int, Int, OpCode) = // Dunno how to make this static lmao
  {
    val opCodeInt: Int = code % 100 // Instruction Code
    val modeStr = code.toString.substring(0, {if (code.toString.length > 1) code.toString.length - 2 else 0})
    val modes = "000".substring(modeStr.length) + modeStr
    (modes(0).asDigit, modes(1).asDigit, modes(2).asDigit, opCodeInt) match
    {
      case (a, b, c, 1) => (a, b, c, OpCode1)
      case (a, b, c, 2) => (a, b, c, OpCode2)
      case (a, b, c, 3) => (a, b, c, OpCode3)
      case (a, b, c, 4) => (a, b, c, OpCode4)
      case (a, b, c, 5) => (a, b, c, OpCode5)
      case (a, b, c, 6) => (a, b, c, OpCode6)
      case (a, b, c, 7) => (a, b, c, OpCode7)
      case (a, b, c, 8) => (a, b, c, OpCode8)
      case (_, _, _, 99) => (0, 0, 0, OpCode99)
      case c => throw new Exception("Wrong instruction code " + c)
    }
  }

  /** Returns correct value, immediate or positional */
  protected[this] def accessor(param: Int, mode: Int, tape: Array[Int]): Int =
  {
    mode match
    {
      case 0 => /* println("Reading "+tape(param)+" from array("+param+")");*/ tape(param)
      case 1 => /*println("Reading "+param+" immediate");*/ param
      case p => throw new Exception("HCF: wrong accessor parameter " + p)
    }
  }

  /** Writes to the tape in a manner specified by the mode */
  protected[this] def writer(param: Int, mode: Int, tape: Array[Int], value: Int): Unit =
  {
    mode match
    {
      case 0 => tape(param) = value//; println("Writing "+value+" to array("+param+")")
      case 1 => throw new Exception("HCF: unimplemented writer parameter 1")
      case p => throw new Exception("HCF: wrong writer parameter " + p)
    }
  }
}

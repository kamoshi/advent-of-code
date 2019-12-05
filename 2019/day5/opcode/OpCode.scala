package day5.opcode

abstract trait OpCode
{
  /** length of an instruction */
  val length: Int

  /** Parse int to full OpCode */
  def parseInt(code: Int): (Int, Int, Int, OpCode) = // Dunno how to make this static lmao
  {
    val opCodeInt: Int = code % 100 // Instruction Code
    val modeStr = code.toString.substring(0, {if (code.toString.length > 1) code.toString.length - 2 else 0})
    val modes = ("000".substring(modeStr.length) + modeStr).toArray.map(_.asDigit) // List of modes
    opCodeInt match {
      case 1 => (modes(0), modes(1), modes(2), OpCode1)
      case 2 => (modes(0), modes(1), modes(2), OpCode2)
      case 3 => (modes(0), modes(1), modes(2), OpCode3)
      case 4 => (modes(0), modes(1), modes(2), OpCode4)
      case 5 => (modes(0), modes(1), modes(2), OpCode5)
      case 6 => (modes(0), modes(1), modes(2), OpCode6)
      case 7 => (modes(0), modes(1), modes(2), OpCode7)
      case 8 => (modes(0), modes(1), modes(2), OpCode8)
      case 99 => (0, 0, 0, OpCode99)
      case c => throw new Exception("Wrong instruction code " + c)
    }
  }

  /** Returns correct value, immediate or positional */
  protected[this] def accessor(param: Int, mode: Int, tape: Array[Int]): Int =
  {
    mode match
    {
      case 0 => /* println("Reading "+tape(param)+" from array("+param+")");*/ tape(param);
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

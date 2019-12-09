package day09.intcode

import day09.intcode.opcode.{Action, Input, Jump, OpCode9, OpCode99, Output}

class Machine(input: Array[Long])
{
  private[this] val original = input.clone()
  private[this] var software = input.clone()

  private[this] var pointer = 0
  private[this] var relative: Long = 0
  private[this] var halted = false

  def isHalted: Boolean = halted

  def reset(): Machine = {
    software = original.clone()
    halted = false
    pointer = 0
    this
  }

  @scala.annotation.tailrec
  final def runUntilIO(): Machine =
  {
    val tuple = OpCode99.parseInt(software(pointer).toInt);
    tuple match
    {
      case (_, _, _, OpCode99) =>
        halted = true
        this
      case (_, _, _, outputInstr: Output) => this
      case (_, _, _, inputInstr: Input) => this
      case (m3, m2, m1, instruction: Jump) =>
        val (bool, jmpPtr) = instruction.checkConditionAndJump(software, relative, software(pointer+1), software(pointer+2), software(pointer+3), m1, m2, m3)
        if (bool) pointer = jmpPtr
        else pointer += instruction.length
        runUntilIO()
      case (m3, m2, m1, instruction: Action) =>
        instruction.exec(software, relative, software(pointer+1), software(pointer+2), software(pointer+3), m1, m2, m3)
        pointer += instruction.length
        runUntilIO()
      case (_, _, m1, OpCode9) =>
        relative += OpCode9.exec(software, relative, software(pointer+1), m1)
        pointer += OpCode9.length
        runUntilIO()
      case _ => throw new Exception("Something went wrong")
    }
  }

  def runInput(input: Long): Machine =
  {
    val tuple = OpCode99.parseInt(software(pointer).toInt)
    tuple match
    {
      case (_, _, _, OpCode99) => this
      case (_, _, m1, inputInstr: Input) =>
        inputInstr.input(software, relative, software(pointer+1), m1, input)
        pointer += inputInstr.length
        this
      case _ => throw new Exception("Unexpected instruction")
    }
  }

  def runOutput(): Long =
  {
    val tuple = OpCode99.parseInt(software(pointer).toInt)
    tuple match
    {
      case (_, _, _, OpCode99) => 0
      case (_, _, m1, outputInstr: Output) =>
        val out = outputInstr.output(software, relative, software(pointer+1), m1)
        pointer += outputInstr.length
        out
      case _ => throw new Exception("Unexpected instruction")
    }
  }
}

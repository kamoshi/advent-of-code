package day07.intcode

import day07.intcode.opcode.{Action, Input, Jump, OpCode99, Output}

class Amplifier(input: Array[Int])
{
  private[this] val original = input.clone()
  private[this] var software = input.clone()

  private[this] var pointer = 0
  private[this] var halted = false

  def isHalted: Boolean = halted

  def reset(): Amplifier = {
    software = original.clone()
    halted = false
    pointer = 0
    this
  }

  def runUntilIO(): Amplifier =
  {
    val tuple = OpCode99.parseInt(software(pointer));
    tuple match
    {
      case (_, _, _, OpCode99) =>
        halted = true
        this
      case (_, _, _, outputInstr: Output) => this
      case (_, _, _, inputInstr: Input) => this
      case (m3, m2, m1, instruction: Jump) =>
        val (bool, jmpPtr) = instruction.checkConditionAndJump(software, software(pointer+1), software(pointer+2), software(pointer+3), m1, m2, m3)
        if (bool) pointer = jmpPtr
        else pointer += instruction.length
        runUntilIO()
      case (m3, m2, m1, instruction: Action) =>
        instruction.exec(software, software(pointer+1), software(pointer+2), software(pointer+3), m1, m2, m3)
        pointer += instruction.length
        runUntilIO()
      case _ => throw new Exception("Something went wrong")
    }
  }

  def runInput(input: Int): Amplifier =
  {
    val tuple = OpCode99.parseInt(software(pointer))
    tuple match
    {
      case (_, _, _, OpCode99) => this
      case (_, _, m1, inputInstr: Input) =>
        inputInstr.input(software, software(pointer+1), m1, input)
        pointer += inputInstr.length
        this
      case _ => throw new Exception("Unexpected instruction")
    }
  }

  def runOutput(): Int =
  {
    val tuple = OpCode99.parseInt(software(pointer))
    tuple match
    {
      case (_, _, _, OpCode99) => 0
      case (_, _, m1, outputInstr: Output) =>
        val out = outputInstr.output(software, software(pointer+1), m1)
        pointer += outputInstr.length
        out
      case _ => throw new Exception("Unexpected instruction")
    }
  }
}

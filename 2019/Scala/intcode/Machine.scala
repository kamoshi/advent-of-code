package intcode

import intcode.opcode.{Action, Input, Jump, OpCode9, OpCode99, Output}

/**
 * Virtual IntCode machine
 * @param input Memory used to initialize the machine with.
 */
class Machine(input: Array[Long])
{
  private[this] val original: Array[Long] = input.clone()  // original memory saved in case of reset
  private[this] var software: Array[Long] = input.clone()  // working memory used by the machine

  private[this] var pointer: Int = 0      // instruction pointer
  private[this] var relative: Long = 0    // relative pointer
  private[this] var state: State = Ready  // machine state

  def isHalted: Boolean = state != Ready
  def isIO: Boolean = state == Input || state == Output
  def isReady: Boolean = state == Ready
  def isInput: Boolean = state == Input
  def isOutput: Boolean = state == Output
  def getState: State = state

  def getMem(ptr: Int): Long =
    software(ptr)

  def setMem(ptr: Int, value: Long): Unit =
    software(ptr) = value

  def reset(): Machine =
  {
    software = original.clone()
    state = Ready
    pointer = 0
    this
  }

  @scala.annotation.tailrec
  final def run(): Machine =
  {
    val tuple = OpCode99.parseInt(software(pointer).toInt)
    tuple match
    {
      case (_, _, _, OpCode99) =>
        state = Finished
        this

      case (_, _, _, outputInstr: Output) =>
        state = Output
        this

      case (_, _, _, inputInstr: Input) =>
        state = Input
        this

      case (m3, m2, m1, instruction: Jump) =>
        val (bool, jmpPtr) = instruction.checkConditionAndJump(software, relative, software(pointer+1), software(pointer+2), software(pointer+3), m1, m2, m3)
        if (bool) pointer = jmpPtr
        else pointer += instruction.length
        state = Ready
        run()

      case (m3, m2, m1, instruction: Action) =>
        instruction.exec(software, relative, software(pointer+1), software(pointer+2), software(pointer+3), m1, m2, m3)
        pointer += instruction.length
        state = Ready
        run()

      case (_, _, m1, OpCode9) =>
        relative += OpCode9.exec(software, relative, software(pointer+1), m1)
        pointer += OpCode9.length
        state = Ready
        run()

      case _ => throw new Exception("Something went wrong")
    }
  }

  def runInput(input: Long): Machine =
  {
    val tuple = OpCode99.parseInt(software(pointer).toInt)
    tuple match
    {
      case (_, _, _, OpCode99) =>
        state = Finished
        this
      case (_, _, m1, inputInstr: Input) =>
        inputInstr.input(software, relative, software(pointer+1), m1, input)
        pointer += inputInstr.length
        state = Ready
        this
      case _ => throw new Exception("Unexpected instruction")
    }
  }

  def runOutput(): Long =
  {
    val tuple = OpCode99.parseInt(software(pointer).toInt)
    tuple match
    {
      case (_, _, _, OpCode99) =>
        state = Finished
        0
      case (_, _, m1, outputInstr: Output) =>
        val out = outputInstr.output(software, relative, software(pointer+1), m1)
        pointer += outputInstr.length
        state = Ready
        out
      case _ => throw new Exception("Unexpected instruction")
    }
  }

  @scala.annotation.tailrec
  final def runContinuous(inputs: List[Long], outputs: List[Long] = List()): List[Long] =
  {
    this.state match
    {
      case Ready =>
        run()
        runContinuous(inputs, outputs)
      case Finished =>
        outputs
      case Output =>
        runContinuous(inputs, this.runOutput()::outputs)
      case Input =>
        if (inputs.isEmpty)
          {
            println("Ran out of inputs")
            outputs
          }
        else
          {
            runInput(inputs.head)
            runContinuous(inputs.tail, outputs)
          }
    }
  }
}

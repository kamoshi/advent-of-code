package intcode

import intcode.opcode.{Action, Input, Jump, OpCode9, OpCode99, Output}

import scala.collection.mutable.ListBuffer

/**
 * Virtual IntCode machine
 * @param input Memory used to initialize the machine with.
 */
class Machine(input: Array[Long])
{
  // Tapes
  private[this] val original: Array[Long] = input.clone()  // original memory saved in case of reset
  private[this] var software: Array[Long] = input.clone()  // working memory used by the machine

  // State
  private[this] var pointer: Int = 0      // instruction pointer
  private[this] var relative: Long = 0    // relative pointer
  private[this] var state: State = Ready  // machine state

  // IO Buffers
  private[this] val inputStream: ListBuffer[Long] = new ListBuffer[Long]
  private[this] val outputStream: ListBuffer[Long] = new ListBuffer[Long]

  def enqueue(number: Long): Machine =
  {
    inputStream.addOne(number)
    this
  }

  def enqueue(list: List[Long]): Machine =
  {
    inputStream.addAll(list)
    this
  }

  def output: Long =
    outputStream.remove(0)

  def outputAll: List[Long] =
  {
    val accumulated = outputStream.result
    outputStream.clear()
    accumulated
  }

  def isHalted: Boolean = state != Ready
  def isReady: Boolean = state == Ready
  def isInput: Boolean = state == Input
  def getState: State = state
  def hasOutput: Boolean = outputStream.nonEmpty

  def getMem(ptr: Int): Long =
    software(ptr)

  def setMem(ptr: Int, value: Long): Unit =
    software(ptr) = value

  def reset(): Machine =
  {
    software = original.clone()
    inputStream.clear()
    outputStream.clear()
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
        println("Machine finished")
        this

      case (_, _, m1, outputInstr: Output) =>
        outputStream.addOne(outputInstr.output(software, relative, software(pointer+1), m1))
        pointer += outputInstr.length
        state = Ready
        run()

      case (_, _, m1, inputInstr: Input) =>
        //buffer is empty
        if (inputStream.isEmpty)
        {
          state = Input
          println("Ran out of inputs")
          this
        }
        // buffer contains inputs
        else
        {
          inputInstr.input(software, relative, software(pointer+1), m1, inputStream.remove(0))
          pointer += inputInstr.length
          state = Ready
          run()
        }

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
}

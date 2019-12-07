package day07.intcode

class Solver(software: Array[Int])
{
  /** Array containing the amplifiers */
  val amps: Array[Amplifier] = Array(new Amplifier(software), new Amplifier(software), new Amplifier(software), new Amplifier(software), new Amplifier(software))

  /** Reset the amplifiers to the original state */
  def reset(): Unit =
    amps.foreach(_.reset())

  /** Inserts phases AKA the first inputs into amplifiers */
  def setupPhases(phases: Array[Int]): Unit =
  {
    amps.foreach(_.runUntilIO())
    for (i <- amps.indices)
    {
      amps(i).runInput(phases(i))
    }
  }

  /** Solves the single run */
  def solveSequenceSingleRun(phases: Array[Int]): Int =
  {
    reset()
    setupPhases(phases)
    amps.foldLeft(0){ (input, amp) => amp.runUntilIO().runInput(input).runUntilIO().runOutput() }
  }

  @scala.annotation.tailrec
  private[this] final def solveSequenceFeedback(lastOutput: Int = 0): Int =
  {
    val result = amps.foldLeft(lastOutput){ (input, amp) => amp.runUntilIO().runInput(input).runUntilIO().runOutput() }
    if (amps.foldLeft(false) {(bool, amp) => bool || amp.isHalted}) lastOutput
    else solveSequenceFeedback(result)
  }
  
  /** Solves the sequence featuring a feedback loop */
  def solveSequenceMultiRun(phases: Array[Int]): Int =
  {
    reset()
    setupPhases(phases)
    solveSequenceFeedback()
  }

}

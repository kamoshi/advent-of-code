package day07.intcode

class Solver(software: Array[Int])
{
  val amps: Array[Amplifier] = Array(new Amplifier(software), new Amplifier(software), new Amplifier(software), new Amplifier(software), new Amplifier(software))

  def reset(): Unit =
    amps.foreach(_.reset())

  def setupPhases(phases: Array[Int]): Unit =
  {
    amps.foreach(_.runUntilIO())
    amps(0).runInput(phases(0))
    amps(1).runInput(phases(1))
    amps(2).runInput(phases(2))
    amps(3).runInput(phases(3))
    amps(4).runInput(phases(4))
  }

  def solveSequence(lastOutput: Int = 0): Int =
  {
    val res0 = amps(0).runUntilIO().runInput(lastOutput).runUntilIO().runOutput()
    val res1 = amps(1).runUntilIO().runInput(res0).runUntilIO().runOutput()
    val res2 = amps(2).runUntilIO().runInput(res1).runUntilIO().runOutput()
    val res3 = amps(3).runUntilIO().runInput(res2).runUntilIO().runOutput()
    val res4 = amps(4).runUntilIO().runInput(res3).runUntilIO().runOutput()
    res4
  }

  def solveSequenceSingleRun(phases: Array[Int]): Int =
  {
    reset()
    setupPhases(phases)
    solveSequence()
  }

  @scala.annotation.tailrec
  final def solveSequenceFeedback(lastOutput: Int = 0): Int =
  {
    val res0 = amps(0).runUntilIO().runInput(lastOutput).runUntilIO().runOutput()
    val res1 = amps(1).runUntilIO().runInput(res0).runUntilIO().runOutput()
    val res2 = amps(2).runUntilIO().runInput(res1).runUntilIO().runOutput()
    val res3 = amps(3).runUntilIO().runInput(res2).runUntilIO().runOutput()
    val res4 = amps(4).runUntilIO().runInput(res3).runUntilIO().runOutput()
    if (amps.foldLeft(false) {(bool, amp) => bool || amp.isHalted}) lastOutput
    else solveSequenceFeedback(res4)
  }

  def solveSequenceMultiRun(phases: Array[Int]): Int =
  {
    reset()
    setupPhases(phases)
    solveSequenceFeedback()
  }

}

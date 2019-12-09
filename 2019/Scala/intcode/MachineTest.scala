package intcode

import kamlib.Reader

object MachineTest
{

  def testDay02(): Unit =
  {
    val input: Array[Long] = Reader.readString("/input2.txt").split("[^\\d-]+").map(x => x.toLong)

    val machine1: Machine = new Machine(input)
    machine1.setMem(1, 12)
    machine1.setMem(2, 2)
    assert(machine1.run().getMem(0) == 9706670)

    val machine2: Machine = new Machine(input)
    machine2.setMem(1, 25)
    machine2.setMem(2, 52)
    assert(machine2.run().getMem(0) == 19690720)
  }

  def testDay05(): Unit =
  {
    val input: Array[Long] = Reader.readString("/input5.txt").split("[^\\d-]+").map(x => x.toLong)
    val machine1: Machine = new Machine(input)
    assert(machine1.enqueue(1).run().outputAll.last == 12234644)

    val machine2: Machine = new Machine(input)
    assert(machine2.enqueue(5).run().output == 3508186)
  }

  def testDay09(): Unit =
  {
    val memory: Array[Long] = Array.ofDim[Long](1200)
    val input: Array[Long] = Reader.readString("/input9.txt").split("[^\\d-]+").map(x => x.toLong)
    for(i <- input.indices) { memory(i) = input(i)}

    val machine1: Machine = new Machine(memory)
    assert(machine1.enqueue(1).run().output == 4288078517L)

    val machine2: Machine = new Machine(memory)
    assert(machine2.enqueue(2).run().output == 69256)
  }

}

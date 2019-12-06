package day06

import scala.collection.mutable

class Day06(input: List[String])
{
  /** Easy access to all nodes */
  val nodes = new mutable.HashMap[String, Node]

  /** list of tuples */
  val tuples: List[(String, String)] = input.map(string => {
    val split = string.split(')')
    (split(0), split(1))
  })

  tuples.foreach(tuple => {
    val (a , b) = tuple
    if (!nodes.contains(a)) nodes.addOne(a, new Node(a))
    if (!nodes.contains(b)) nodes.addOne(b, new Node(b))
  })

  tuples.foreach(tuple => {
    val (a , b) = tuple
    nodes(a).children += nodes(b)
    nodes(b).parent = nodes(a)
  })

  def solveP1(): Int =
  {
    val start: Node = nodes("COM")
    val counter = new Counter
    start.countDepths(0, counter)
    counter.count
  }

  def solveP2(): Int =
  {
    val start = nodes("YOU")
    val end = nodes("SAN")
    val youParents = start.getParents
    val mergePoint = end.findFirstCommon(youParents)
    start.findDistanceToParent(mergePoint) + end.findDistanceToParent(mergePoint)
  }

}

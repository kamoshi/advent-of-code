package day06

import scala.collection.mutable

sealed trait Tree
case object Leaf extends Tree
case class Node(name: String, var parent: Tree = Leaf) extends Tree
{
  val children = new mutable.ArrayBuffer[Node]

  def countDepths(depth: Int, counter: Counter): Unit =
  {
    counter.inc(depth)
    children.foreach(child => {
      child.countDepths(depth+1, counter)
    })
  }

  def getParents: List[Node] =
    parent match
    {
      case Leaf => List()
      case p: Node => p::p.getParents
    }

  def findFirstCommon(nodes: List[Node]): Node =
  {
    parent match
    {
      case Leaf => throw new Exception("Something went wrong")
      case p: Node => if (nodes.contains(p)) p else p.findFirstCommon(nodes)
    }
  }

  def findDistanceToParent(target: Node, acc: Int = 0): Int =
  {
    parent match
    {
      case Leaf => throw new Exception("Something went wrong")
      case p: Node => if (p == target) acc else p.findDistanceToParent(target, acc+1)
    }
  }
}
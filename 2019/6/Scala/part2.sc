package day6

import scala.collection.mutable
import scala.io.Source

case class Node(name: String)
{
  val connections = new mutable.ArrayBuffer[Node]

  def markVisited(depth: Int, visited: mutable.HashSet[Node], distances: mutable.HashMap[String, Int]): Unit =
  {
    connections.foreach(x => {
      if (!visited.contains(x))
      {
        visited.add(x)
        distances.addOne(x.name, depth)
        x.markVisited(depth+1, visited, distances)
      }
    })
  }
}

object Main {

  /** list of tuples */
  val input: List[(String, String)] = Source.fromFile(getClass.getResource("/input6.txt").getFile).getLines().map(x =>
    {
      val split = x.split(')')
      (split(0), split(1))
    }).toList

  /** Easy access to all nodes */
  val nodes = new mutable.HashMap[String, Node]
  input.foreach(tuple =>
  {
    val (a , b) = tuple
    if (!nodes.contains(a)) nodes.addOne(a, new Node(a))
    if (!nodes.contains(b)) nodes.addOne(b, new Node(b))
  })

  input.foreach(tuple =>
  {
    val (a , b) = tuple
    nodes(a).connections += nodes(b)
    nodes(b).connections += nodes(a)
  })

  /** Visited nodes */
  val visited = new mutable.HashSet[Node]

  /** Easy access to all nodes */
  val distances = new mutable.HashMap[String, Int]

  def main(args: Array[String]): Unit = {
    println("Starting program...")

    val start = nodes("YOU")
    start.markVisited(0, visited, distances)
    println(distances("SAN") - 1)

    println("Program finished...")
  }
}

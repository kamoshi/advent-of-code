package day10

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class Day10(input: List[List[Char]])
{
  // Set of all asteroids represented by vectors from (0, 0)
  val set: mutable.HashSet[Vector2] = new mutable.HashSet[Vector2]

  // Parse input into the set
  for (y <- input.indices; x <- input(0).indices) {
    val char = input(y)(x)
    if (char == '#') set.add(Vector2(x, y))
  }

  /** Solve part 1 */
  def solveP1(): (Vector2, Int) =
  {
    // map each asteroid to number of other asteroid it can "see"
    val map = new mutable.HashMap[Vector2, Int]
    set.foreach(x => map.addOne(x, x.raycastSet(set).size))
    map.toList.maxBy(_._2) // find the one with the most seen
  }

  def solveP2(): Vector2 =
  {
    //val center = solveP1()._1 // center
    val center = Vector2(29,28) // when you assume you already know the answer

    // Map angle to *queue* of points (multiple points can have same angle)
    val angleMap = new mutable.HashMap[Vector2, ListBuffer[Vector2]]

    // Write to the queues
    set.foreach(x => {
      val v2FromCenter = center.raycast(x)
      if (angleMap.contains(v2FromCenter)) {
        angleMap(v2FromCenter).addOne(center.toVector(x)) // Append to the end
      }
      else { // Doesn't exist -> add entry
        angleMap.addOne(v2FromCenter, ListBuffer(center.toVector(x)))
      }
    })
    angleMap.remove(Vector2(0,0)) // this is the center, we don't need it
    angleMap.foreachEntry((_, b) => b.sortWith(_.distance < _.distance)) // Sort queues by distance

    // get list of angles sorted from Y axis
    def rayOrder():List[Vector2] = angleMap.keys.toList.sortWith(_.angleFromY < _.angleFromY)

    // Find n-th deletion
    def findNthDeletion(n: Int): Vector2 = {
      var removals = 0
      var lookedFor = Vector2(0,0)

      def cyclicDelete(): Unit = {
        val steps = rayOrder()
        for(i <- steps.indices) {
          removals += 1
          val buffer = angleMap(steps(i))
          val removedP = buffer.remove(0) // remove from queue the closest asteroid
          if (removals == n) lookedFor = removedP // this is the one were looking for
          if (buffer.isEmpty) // if queue is empty -> no more asteroids sharing a normal vector; remove key
          {
            angleMap.remove(steps(i))
          }
        }
      }
      while (angleMap.nonEmpty && removals < n) {
        cyclicDelete()
      }
      center + lookedFor
    }

    findNthDeletion(200)
  }
}

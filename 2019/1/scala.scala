import scala.io.Source
val lines = Source.fromFile(getClass.getResource("/input1.txt").getFile).getLines().toList

// 1
val result1 = lines.foldLeft (0) { (acc, i) => acc + (i.toInt/3-2)}
println(result1)

// 2
@scala.annotation.tailrec
def findRecursiveFuel(fuel:Int, acc:Int):Int =
{
  val nextFuel = (fuel/3)-2
  if (nextFuel <= 0) acc
  else findRecursiveFuel(nextFuel, nextFuel+acc)
}

val result2 = lines.foldLeft (0) { (acc, i) => acc + findRecursiveFuel(i.toInt, 0)}
println(result2)

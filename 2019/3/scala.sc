import scala.collection.mutable
import scala.io.Source

sealed trait Direction
case object Up extends Direction
case object Down extends Direction
case object Left extends Direction
case object Right extends Direction

case class Command(dir: Direction, num: Int)
case class Point(x: Int, y: Int)
{
  def xMod(int: Int):Point = Point(x+int, y)
  def yMod(int: Int):Point = Point(x, y+int)
  def sum: Int = x.abs + y.abs
}

def stringToCommand(string: String): Command =
  (string.charAt(0), string.substring(1).toInt) match
  {
    case ('U', num) => Command(Up, num)
    case ('D', num) => Command(Down, num)
    case ('L', num) => Command(Left, num)
    case ('R', num) => Command(Right, num)
    case _ => throw new Exception("Wrong command")
  }

val paths = Source.fromFile(getClass.getResource("/input3.txt").getFile).getLines().toArray
val path1 = paths(0).mkString.split(",").toList.map(s => stringToCommand(s))
val path2 = paths(1).mkString.split(",").toList.map(s => stringToCommand(s))

def commandToPoints(command: Command, point: Point, acc: List[Point]):(Point, List[Point]) =
{
  command match {
    case Command(_, 0) => (point, acc)
    case Command(Up, num) => commandToPoints(Command(Up, num-1), point.yMod(1), point.yMod(1) :: acc)
    case Command(Down, num) => commandToPoints(Command(Down, num-1), point.yMod(-1), point.yMod(-1) :: acc)
    case Command(Left, num) => commandToPoints(Command(Left, num-1), point.xMod(-1), point.xMod(-1) :: acc)
    case Command(Right, num) => commandToPoints(Command(Right, num-1), point.xMod(1), point.xMod(1) :: acc)
  }
}

def findAllPoints(listOfCommands: List[Command], lastPoint: Point = Point(0,0), acc:List[Point] = List()): List[Point] =
  listOfCommands match
  {
    case List() => acc
    case x::xs =>
    {
      val (resPoint, resList) = commandToPoints(x, lastPoint, List());
      findAllPoints(xs, resPoint, resList:::acc)
    }
  }

val set = new mutable.HashSet[Point]()          // set with all points from first wire
findAllPoints(path1).foreach(p => set.add(p))

def findIntersect(otherPoints: List[Point]): List[Point] =
  otherPoints match
  {
    case List() => List()
    case x::xs =>if (set.contains(x)) x::findIntersect(xs) else findIntersect(xs)
  }

val intersecting = findIntersect(findAllPoints(path2))
val bestPointPart1 = intersecting.tail.foldLeft (intersecting.head) {(old, next) => if (next.sum < old.sum) next else old}

println(bestPointPart1)

// ===================== PART 2 ======================== //
val map = mutable.Map.empty[Point, Int]

def markStepsAtIntercepts(pointsPath: List[Point], step: Int = 1): Unit =
{
  pointsPath match
  {
    case List() => ()
    case x::xs =>
    {
      if (intersecting.contains(x))
      {
        if (map.contains(x)) map(x) = step + map(x)
        else map.addOne(x, step)
      }
      markStepsAtIntercepts(xs, step + 1)
    }
  }
}

markStepsAtIntercepts(findAllPoints(path1).reverse)
markStepsAtIntercepts(findAllPoints(path2).reverse)

val converted = map.toList
val result2 = (converted.tail.foldLeft (converted.head) {(old, next) => if (old._2 > next._2) next else old})._2
